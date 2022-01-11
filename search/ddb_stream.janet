(import ./../aws_api/aws_api :as aws-api)

(def TABLE_NAME "janet_postings")

(defn make-ddbs-client []
  (let [ddb-client (aws-api/make-ddb-client)

        endpoint (get ddb-client :endpoint)]
    @{:endpoint endpoint
      :service "dynamodb" # signing-name is dynamodb, service name is streams-dynamodb
      :target-prefix "DynamoDBStreams_20120810"}))

(defn read-stream [ddbs-client out-chan]
  (let [step! (fn [last-seq-num]
                #(printf "step! last-seq-num %P" last-seq-num)
                (let [streams (do (var last-stream-arn nil)
                                  (def out @[])
                                  (while (def desc (aws-api/invoke ddbs-client
                                                                   {:op :ListStreams
                                                                    :request {"TableName" "janet_postings"
                                                                              "ExclusiveStartStreamArn" last-stream-arn}}))
                                    # (printf "list-streams desc: %P" desc)
                                    (array/concat out (get desc "Streams"))
                                    (if-let [next-last-stream-arn (get desc "LastEvaluatedStreamArn")]
                                      (set last-stream-arn next-last-stream-arn)
                                      (break)))
                                  out)

                      stream-arn (-> streams
                                     first
                                     (get "StreamArn"))

                      descs (do (var last-shard-id nil)
                                (def out @[])
                                (while (def desc (aws-api/invoke ddbs-client
                                                                 {:op :DescribeStream
                                                                  :request {"StreamArn" stream-arn
                                                                            "ExclusiveStartShardId" last-shard-id}}))
                                  #(printf "desc: %P" desc)
                                  (array/concat out (get-in desc ["StreamDescription" "Shards"]))
                                  (if-let [next-last-shard-id (get-in desc ["StreamDescription" "LastEvaluatedShardId"])]
                                    (set last-shard-id next-last-shard-id)
                                    (break)))
                                out)

                      has-or-after-seq-num (fn [shard last-seq-num]
                                             (if-not last-seq-num
                                               shard # do all shards if none done yet
                                               (let [{"StartingSequenceNumber" starting-sequence-number
                                                      "EndingSequenceNumber" ending-sequence-number} (get shard "SequenceNumberRange")]
                                                 # SequenceNumber are zero padded strings: 000000000000000000339
                                                 (when (if ending-sequence-number
                                                         (and starting-sequence-number ending-sequence-number
                                                              # (<= StartingSequenceNumber last-seq-num EndingSequenceNumber)
                                                              (and (<= (compare starting-sequence-number last-seq-num) 0)
                                                                   (<= (compare last-seq-num ending-sequence-number) 0)))
                                                         (and starting-sequence-number
                                                              # (<= StartingSequenceNumber last-seq-num)
                                                              (<= (compare starting-sequence-number last-seq-num) 0)))
                                                   shard))))

                      shards (keep (fn [shard]
                                     (has-or-after-seq-num shard last-seq-num))
                                   descs)

                      records (do (var out @[])
                                  (each shard shards
                                    (let [shard-id (get shard "ShardId")
                                          iter (aws-api/invoke ddbs-client
                                                               {:op :GetShardIterator
                                                                :request (merge-into @{"StreamArn" stream-arn
                                                                                       "ShardId" shard-id}
                                                                                     (if last-seq-num
                                                                                       {"ShardIteratorType" "AFTER_SEQUENCE_NUMBER"
                                                                                        "SequenceNumber" last-seq-num}
                                                                                       # from start
                                                                                       {"ShardIteratorType" "TRIM_HORIZON"}))})
                                          #_ (printf "iter %P" iter)
                                          shard-iterator-arn (get iter "ShardIterator")
                                          closed (if (get-in shard ["SequenceNumberRange" "EndingSequenceNumber"])
                                                   true
                                                   false)]
                                      (do (var shard-iterator-arn shard-iterator-arn)
                                          (while (def result (aws-api/invoke ddbs-client
                                                                             {:op :GetRecords
                                                                              :request {"Limit" 5
                                                                                        "ShardIterator" shard-iterator-arn}}))
                                            #(printf "get-records %P" result)
                                            (when (get result :error) # assume TrimmedDataAccessException
                                              (break))
                                            (def records (get result "Records"))
                                            (array/concat out records)
                                            (if-let [next-shard-iterator-arn (if closed # a closed shard should be followed by an open shard
                                                                               (get result "NextShardIterator")
                                                                               # do loop for an empty open ended iterator
                                                                               (when (< 0 (length records))
                                                                                 (get result "NextShardIterator")))]
                                              (set shard-iterator-arn next-shard-iterator-arn)
                                              (break))
                                            ))))
                                  out)
                      ]
                  records
                  ))]
    (do (var last-seq-num nil)
        (while (def records (step! last-seq-num))
          (printf "step! res: %P" (length records))
          (if (< 0 (length records))
            (do (ev/give out-chan records)
                (set last-seq-num (-> records
                                      last
                                      (get-in ["dynamodb" "SequenceNumber"])))
                # describe stream is throttled to 10 times per second
                (ev/sleep 1))
            (break)
            )))
    ))

(comment
 # page with :request {"ExclusiveStartStreamArn" stream-arn} and "LastEvaluatedStreamArn" in result
 (aws-api/invoke ddbs-client
                 {:op :ListStreams
                  :request {"TableName" "janet_postings"}})
 (os/cd "..")
 (os/cwd)
 (def ddbs-client (make-ddbs-client))
 
 (import ./search/import :as search-import :fresh true)
 
 (def rc (ev/chan 100))
 (read-stream ddbs-client rc)
 (while (def records (ev/with-deadline 1 (ev/take rc)))
   (printf "Count recs: %P" (length records))
   # wrap {"Records" ..} to look like lambda event
   (search-import/handle-import @{"Records" records})
   )

 (def ddb-client (table (splice (kvs ddbs-client))
                        :target-prefix "DynamoDB_20120810"))
 
 (do (var k nil)
     (while (def result (aws-api/invoke ddb-client
                                        {:op :Scan
                                         :request {"TableName" "janet_postings"
                                                   "ExclusiveStartKey" k}}))
       (let [items (get result "Items")]
         (when (< 0 (length items))
           (search-import/handle-import @{"Records" (map (fn [image]
                                                           {"dynamodb" {"NewImage" image}})
                                                         items)})))
       (if-let [next-k (get result "LastEvaluatedKey")]
         (set k next-k)
         (break))))
 # Count Items LastEvaluatedKey (table of attrs!!)
 
 )

# stream-res (aws-api/invoke ddbs-client
#                        {:op :ListStreams
#                         :request {"TableName" "janet_postings"}})
# @{"Streams" @[@{"StreamArn" "arn:aws:dynamodb:ddblocal:000000000000:table/janet_postings/stream/2022-01-06T18:07:44.275" "StreamLabel" "2022-01-06T18:07:44.275" "TableName" "janet_postings"}]}


# Page with ExclusiveStartShardId & StreamDescription LastEvaluatedShardId
# describe-result (aws-api/invoke ddbs-client
#                             {:op :DescribeStream
#                              :request {"StreamArn" stream-arn}})
#@{"StreamDescription" @{ "CreationRequestDateTime" 1.64149e+09 "KeySchema" @[@{"AttributeName" "pk" "KeyType" "HASH"} @{"AttributeName" "sk" "KeyType" "RANGE"}] "Shards" @[@{"SequenceNumberRange" @{"StartingSequenceNumber" "000000000000000000002"} "ShardId" "shardId-00000001641492464294-49467681"}] "StreamArn" "arn:aws:dynamodb:ddblocal:000000000000:table/janet_postings/stream/2022-01-06T18:07:44.275" "StreamLabel" "2022-01-06T18:07:44.275" "StreamStatus" "ENABLED" "StreamViewType" "NEW_IMAGE" "TableName" "janet_postings"}}
# Shard with StartingSequenceNumber & EndingSequenceNumber is closed

# @{"ShardIterator" "000|arn:aws:dynamodb:ddblocal:000000000000:table/janet_postings/stream/2022-01-06T18:07:44.275|c2hhcmRJZC0wMDAwMDAwMTY0MTQ5MjQ2NDI5NC00OTQ2NzY4MXwwMDAwMDAwMDAwMDAwMDAwMDAwMDJ8MDAwMDAwMDAwMDAwMDAwMDAxNjQxNDk1MzA4MDE0"}
# @{"NextShardIterator" "000|arn:aws:dynamodb:ddblocal:000000000000:table/janet_postings/stream/2022-01-06T18:07:44.275|c2hhcmRJZC0wMDAwMDAwMTY0MTQ5MjQ2NDI5NC00OTQ2NzY4MXwwMDAwMDAwMDAwMDAwMDAwMDAwMDV8MDAwMDAwMDAwMDAwMDAwMDAxNjQxNDk1NDI4Njcx" "Records" @[@{"awsRegion" "ddblocal" "dynamodb" @{ "ApproximateCreationDateTime" 1641492480 "Keys" @{"pk" @{"S" "0Msk_67EUu3YgNPVII_f"} "sk" @{"S" "A##POST"}} "NewImage" @{ "cdate" @{"S" "20220106T180857Z"} "comment-count" @{"N" "0"} "gsi1pk" @{"S" "POST"} "gsi1sk" @{"S" "20220106T180857Z_0Msk_67EUu3YgNPVII_f"} "pk" @{"S" "0Msk_67EUu3YgNPVII_f"} "sk" @{"S" "A##POST"} "text" @{"S" "First post body!"} "title" @{"S" "First post!"} "username" @{"S" "Guest 799283152"} "userslug" @{"S" "guest799283152"}} "SequenceNumber" "000000000000000000002" "SizeBytes" 231 "StreamViewType" "NEW_IMAGE"} "eventID" "d0228245-774f-4941-a1e4-7b8e5d3d9691" "eventName" "INSERT" "eventSource" "aws:dynamodb" "eventVersion" "1.1"} @{"awsRegion" "ddblocal" "dynamodb" @{ "ApproximateCreationDateTime" 1641492540 "Keys" @{"pk" @{"S" "0Msk_67EUu3YgNPVII_f"} "sk" @{"S" "A##POST"}} "NewImage" @{ "cdate" @{"S" "20220106T180857Z"} "comment-count" @{"N" "1"} "gsi1pk" @{"S" "POST"} "gsi1sk" @{"S" "20220106T180857Z_0Msk_67EUu3YgNPVII_f"} "pk" @{"S" "0Msk_67EUu3YgNPVII_f"} "sk" @{"S" "A##POST"} "text" @{"S" "First post body!"} "title" @{"S" "First post!"} "username" @{"S" "Guest 799283152"} "userslug" @{"S" "guest799283152"}} "SequenceNumber" "000000000000000000003" "SizeBytes" 232 "StreamViewType" "NEW_IMAGE"} "eventID" "d7940182-fffb-4d49-9b5e-45253a1c08bc" "eventName" "MODIFY" "eventSource" "aws:dynamodb" "eventVersion" "1.1"} @{"awsRegion" "ddblocal" "dynamodb" @{ "ApproximateCreationDateTime" 1641492540 "Keys" @{"pk" @{"S" "0Msk_67EUu3YgNPVII_f"} "sk" @{"S" "B#0Msk_AH6J5qDryTodl9Z##COMMENT"}} "NewImage" @{ "cdate" @{"S" "20220106T180918Z"} "pk" @{"S" "0Msk_67EUu3YgNPVII_f"} "sk" @{"S" "B#0Msk_AH6J5qDryTodl9Z##COMMENT"} "text" @{"S" "And the first comment"} "username" @{"S" "Guest 799283152"} "userslug" @{"S" "guest799283152"}} "SequenceNumber" "000000000000000000004" "SizeBytes" 201 "StreamViewType" "NEW_IMAGE"} "eventID" "f068941c-cf57-4166-84d2-8e567da81314" "eventName" "INSERT" "eventSource" "aws:dynamodb" "eventVersion" "1.1"}]}
