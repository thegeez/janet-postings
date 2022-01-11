(import json)
(import ./aws_api/aws_api :as aws-api)
(import ./data_search :as data-search)

(defn main-import [request]
  # does scan of ddb table and post to search for import (as if from ddb stream)
  # will need longer runtime configuration on lambda if number of results becomes large

  (eprintf "Doing maintenance main %p" request)
  (let [lambda-client (aws-api/make-lambda-client)

        function-arn (data-search/get-function-arn)

        invoke-lambda (fn [lambda-request]
                        (try
                          (aws-api/invoke lambda-client
                                          {:op :Invoke
                                           :uri (string "/2015-03-31/functions/" function-arn "/invocations")
                                           :request lambda-request})
                          ([err fib]
                           {:error (string/format "Invoke lambda error: %p" err)
                            :trace (let [b @""]
                                     (with-dyns [:err b]
                                       (debug/stacktrace fib err))
                                     (string b))})))

        ddb-client (aws-api/make-ddb-client)
        responses @[]]
    (do (var k nil)
        (while (def result (aws-api/invoke ddb-client
                                           {:op :Scan
                                            :request {"TableName" "janet_postings"
                                                      "ExclusiveStartKey" k}}))
          (let [items (get result "Items")]
            (when (< 0 (length items))
              (let [request-json (-> (json/encode @{"Records" (map (fn [image]
                                                                     {"dynamodb" {"NewImage" image}})
                                                                   items)})
                                     string)
                    response (invoke-lambda request-json)]
                (array/push responses response))))
          (if-let [next-k (get result "LastEvaluatedKey")]
            (set k next-k)
            (break))))
    (json/encode responses)))

(defn main [request]
  # does scan of ddb table and post to search for import (as if from ddb stream)
  # will need longer runtime configuration on lambda if number of results becomes large

  (eprintf "Doing maintenance main %p" request)
  (let [lambda-client (aws-api/make-lambda-client)

        function-arn (data-search/get-function-arn)

        invoke-lambda (fn [lambda-request]
                        (try
                          (aws-api/invoke lambda-client
                                          {:op :Invoke
                                           :uri (string "/2015-03-31/functions/" function-arn "/invocations")
                                           :request lambda-request})
                          ([err fib]
                           {:error (string/format "Invoke lambda error: %p" err)
                            :trace (let [b @""]
                                     (with-dyns [:err b]
                                       (debug/stacktrace fib err))
                                     (string b))})))


        #request-json "{\"report\": true}"
        request-json "{\"Search\": true, \"query\": \"post\"}"
        response (invoke-lambda request-json)]
    (json/encode response)))

(comment
 (do (each module-path ["profile.janet" "auth.janet" "data.janet" "data_search.janet" "posts.janet" "search/search.janet" "aws_api/aws_api.janet"]
       (put module/cache module-path nil) # force fresh import for module-path
       )
     (eval-string (slurp "service.janet")))

 (def lambda-client (make-lambda-client))
 (aws-api/invoke lambda-client
                 {:op :Invoke
                  :uri (string "/2015-03-31/functions/" function-arn "/invocations")
                  :request payload})
 )
