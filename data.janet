(import ./aws_api/aws_api :as aws-api)
(import ./moment)

(def TABLE_NAME "janet_postings")

(def sk-post-first-ordered "A##POST")
(def gsi1pk-post "POST")
(def sk-comment-prefix "B") # orders behind A##POST
(def sk-comment-postfix "##COMMENT")

(def sk-auth "#USER_ACCOUNT")

(def client-box @[nil])

(defn get-client []
  (if-let [client (get client-box 0)]
    client
    (let [client (aws-api/make-ddb-client)]
      (put client-box 0 client)
      client)))

(defn put-post [post]
  (let [{:slug slug
         :userslug userslug
         :username username
         :created-at created-at
         :title title
         :text text} post]
    (assert slug "put-post slug")
    (assert userslug "put-post userslug")
    (assert username "put-post username")
    (assert created-at "put-post created-at")
    (assert title "put-post title")
    (assert text "put-post text")
    (let [client (get-client)
          pk slug
          sk sk-post-first-ordered

          cdate (aws-api/amz-date-format created-at)
          gsi1pk gsi1pk-post
          gsi1sk (string cdate "_" slug)]
      (aws-api/invoke client
                      {:op :PutItem
                       :request {:TableName TABLE_NAME
                                 :ConditionExpression "attribute_not_exists(#pk)" # in case pushid collision
                                 :ExpressionAttributeNames {"#pk" "pk"}
                                 :Item {:pk {:S pk}
                                        :sk {:S sk}
                                        :userslug {:S userslug}
                                        :username {:S username}
                                        :cdate {:S cdate}
                                        :title {:S title}
                                        :text {:S text}
                                        :comment-count {:N "0"}
                                        :gsi1pk {:S gsi1pk}
                                        :gsi1sk {:S gsi1sk}}}}))))

(defn put-comment [comment]
  (let [{:post-slug post-slug
         :comment-order-id comment-order-id
         :userslug userslug
         :username username
         :created-at created-at
         :text text} comment]
    (assert post-slug "put-comment post-slug")
    (assert comment-order-id "put-comment comment-order-id")
    (assert userslug "put-comment userslug")
    (assert username "put-comment username")
    (assert created-at "put-comment created-at")
    (assert text "put-comment text")
    (let [client (get-client)
          pk post-slug
          sk (string comment-order-id sk-comment-postfix)

          cdate (aws-api/amz-date-format created-at)]
      (try
        # TODO combine into BatchRequest
        (aws-api/invoke client
                        {:op :UpdateItem
                         :request {:TableName TABLE_NAME
                                   :Key {:pk {:S pk}
                                         :sk {:S sk-post-first-ordered}}
                                   :UpdateExpression "ADD #comment_count :inc"
                                   :ExpressionAttributeNames {"#comment_count" "comment-count"}
                                   :ExpressionAttributeValues {":inc" {:N "1"}}
                                   }})
        ([err]
         nil       # keeping comment count correct is best effort only
         ))
      (aws-api/invoke client
                      {:op :PutItem
                       :request {:TableName TABLE_NAME
                                 :ConditionExpression "attribute_not_exists(#pk)" # in case pushid collision
                                 :ExpressionAttributeNames {"#pk" "pk"}
                                 :Item {:pk {:S pk}
                                        :sk {:S sk}
                                        :userslug {:S userslug}
                                        :username {:S username}
                                        :cdate {:S cdate}
                                        :text {:S text}}}}))))

(defn ddb-items->posts&comments [items]
  (let [now (os/time)]
    (seq [item :in items]
         (let [cdate (get-in item ["cdate" "S"])

               item-slug (get-in item ["pk" "S"])

               sk (get-in item ["sk" "S"])

               comment-path (if (string/has-prefix? "B#" sk) # also has-postfix? "##COMMENT"
                              (let [parts (string/split "#" sk)
                                    path (array/slice
                                          parts
                                          1 # drop B#
                                          -3 # drop own commentid & ##COMMENT, so we are left with the parent path
                                          )])
                              [])
               comment-order-id (string (string/join comment-path "-")) # - never in pushid

               item {:slug item-slug
                     :comment-count (get-in item ["comment-count" "N"]) # posts only
                     :comment-order-id comment-order-id # comments only
                     :nesting-path (array/slice comment-path 0 -1) # drop own comment-id, comments only
                     :userslug (get-in item ["userslug" "S"])
                     :username (get-in item ["username" "S"])
                     :title (get-in item ["title" "S"]) # posts only
                     :text (get-in item ["text" "S"])
                     :cdate cdate
                     :created_at_ago (moment/time-ago-str (moment/amz-time-to-seconds cdate) now)}]
           item))))

(defn search-items->posts&comments [items]
  (let [now (os/time)]
    (seq [item :in items]
         (let [cdate (get item "cdate")

               item-slug (get item "pk")

               sk (get item "sk")

               item {:type (get item "type")
                     :slug item-slug
                     :comment-count (get item "comment_count") # posts only
                     :userslug (get item "userslug")
                     :username (get item "username")
                     :title_highlight (get item "title_highlight") # posts only
                     :text_highlight (get item "text_highlight")
                     :post_title (get item "post_title") # comments only
                     :cdate cdate
                     :created_at_ago (when cdate
                                       (moment/time-ago-str (moment/amz-time-to-seconds cdate) now))}]
           item))))

(defn get-posts [before after limit]
  (let [client (get-client)

        # sk of last item prev or first possible val to start from beginning
        after (when (= nil before)
                (or after
                    "9999999999"))
        posts-res (if before
                    (-> (aws-api/invoke client
                                        {:op :Query
                                         :request {:TableName TABLE_NAME
                                                   :IndexName "gsi1"
                                                   :KeyConditionExpression    "#gsi1pk = :post_gsi1pk and #gsi1sk > :start_from"
                                                   :ExpressionAttributeNames {"#gsi1pk" "gsi1pk"
                                                                              "#gsi1sk" "gsi1sk"}
                                                   :ExpressionAttributeValues {":post_gsi1pk" {:S gsi1pk-post}
                                                                               ":start_from" {:S before}
                                                                               }
                                                   :ScanIndexForward true
                                                   :Limit limit}})
                        (update "Items" reverse))
                    # after
                    (aws-api/invoke client
                                    {:op :Query
                                     :request {:TableName TABLE_NAME
                                               :IndexName "gsi1"
                                               :KeyConditionExpression    "#gsi1pk = :post_gsi1pk and #gsi1sk < :start_from"
                                               :ExpressionAttributeNames {"#gsi1pk" "gsi1pk"
                                                                          "#gsi1sk" "gsi1sk"}
                                               :ExpressionAttributeValues {":post_gsi1pk" {:S gsi1pk-post}
                                                                           ":start_from" {:S after}
                                                                           }
                                               :ScanIndexForward false
                                               :Limit limit}}))]
    (if-let [items (get posts-res "Items")]
      (ddb-items->posts&comments items)
      [])))

(defn get-post+comments [post-slug]
  (let [client (get-client)
        pk post-slug
        sk sk-post-first-ordered
        post-res (aws-api/invoke client
                                 {:op :Query
                                  :request {:TableName TABLE_NAME
                                            :KeyConditionExpression    "#pk = :pk and #sk >= :sk"
                                            :ExpressionAttributeNames {"#pk" "pk"
                                                                       "#sk" "sk"}
                                            :ExpressionAttributeValues {":pk" {:S pk}
                                                                        ":sk" {:S sk}
                                                                        }}})]
    (if (= (get post-res "Count") 0)
      nil
      (let [post (-> (take 1 (get post-res "Items"))
                     ddb-items->posts&comments
                     first)
            comments (-> (drop 1 (get post-res "Items"))
                         ddb-items->posts&comments)]
        (merge-into @{:comments comments}
                    post)))))


(defn get-auth [userslug]
  (let [client (get-client)
        # @{} notfound, @{"Item" {"attr" {"S" "..." }}} foudn, @{:error true ...} error
        res (aws-api/invoke client
                            {:op :GetItem
                             :request {:TableName TABLE_NAME
                                       :Key {:pk {:S userslug}
                                             :sk {:S sk-auth}}}})]
    (if (or (empty? res)
            (get res :error))
      {:error true}
      (let [item (get res "Item")
            auth {:userslug (get-in item ["pk" "S"])
                  :username (get-in item ["username" "S"])
                  :roles (let [t @{}]
                           (loop [role-name :in (get-in item ["roles" "SS"])]
                             (put t role-name true))
                           t)
                  :password-encrypted (get-in item ["password-encrypted" "S"])}]
        auth))))

(defn put-auth [auth]
  (let [{:userslug userslug
         :username username
         :password-encrypted password-encrypted
         :roles roles} auth]
    (assert userslug)
    (assert username)
    (assert password-encrypted)
    (assert (< 0 (length roles)))
    (let [client (get-client)

          pk userslug
          sk sk-auth

          created-at-date (aws-api/amz-date-format (os/date))

          roles-ss (seq [role :keys roles]
                        (string role))

          ddb-item {:pk {:S pk}
                    :sk {:S sk}
                    :username {:S username}
                    :password-encrypted {:S password-encrypted}
                    :created-at-date {:S created-at-date}
                    :roles {:SS roles-ss}
                    }]
      (aws-api/invoke client
                      {:op :PutItem
                       :request {:TableName TABLE_NAME
                                 :ConditionExpression "attribute_not_exists(#pk)" # in case pushid collision
                                 :ExpressionAttributeNames {"#pk" "pk"}
                                 :Item ddb-item}}))))

(defn get-profile [userslug]
  (let [client (get-client)
        # @{} notfound, @{"Item" {"attr" {"S" "..." }}} foudn, @{:error true ...} error
        res (aws-api/invoke client
                            {:op :GetItem
                             :request {:TableName TABLE_NAME
                                       :Key {:pk {:S userslug}
                                             :sk {:S sk-auth}}}})]
    (if (or (empty? res)
            (get res :error))
      {:error true}
      (let [item (get res "Item")
            created-at-date (get-in item ["created-at-date" "S"])
            profile {:userslug (get-in item ["pk" "S"])
                     :username (get-in item ["username" "S"])
                     :created-at-date created-at-date
                     :created-at-ago (moment/time-ago-str (moment/amz-time-to-seconds created-at-date) (os/time))
                     }]
        profile))))


(comment
 (let [client (get-client)
       pk "0Mn3F83RJ5qDryTodl9Z"]
   (aws-api/invoke client
                   {:op :UpdateItem
                    :request {:TableName TABLE_NAME
                              :Key {:pk {:S pk}
                                    :sk {:S sk-post-first-ordered}}
                              :UpdateExpression "ADD #comment_count :inc"
                              :ExpressionAttributeNames {"#comment_count" "comment-count"}
                              :ExpressionAttributeValues {":inc" {:N "1"}}
                              }}))

 (let [client (get-client)
       pk "0Mn3F83RJ5qDryTodl9Z"]
   (aws-api/invoke client
                   {:op :GetItem
                    :request {:TableName TABLE_NAME
                              :Key {:pk {:S pk}
                                    :sk {:S sk-post-first-ordered}}
                              }}))
 )
