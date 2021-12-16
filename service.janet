(import /lib/http)
(import json)
(import ./posts)
(import ./profile)
(import ./lib/aws_dynamo :as ddb)
(import ./crypto)
(import ./auth)

(defn file-handler [request root-folder]
  (try
    (let [root (os/realpath root-folder)
          path (get request :uri)
          # chop of public from :uri
          within-path (string/replace "/public" "" path)
          # check if the request does not escape the root dir path
          file-path (string root-folder within-path)
          _ (when (not= (string root within-path)
                        (os/realpath file-path))
              (error "trying to reach file outside of rootfolder"))
          #_ (printf "filehandler getting %p" file-path)
          file-buf (slurp file-path)]
      {:status 200
       :headers {"Cache-Control" (string "max-age=" (* 365 24 60 60))}
       :file-buf file-buf
       :file-path file-path}
      )
    ([err]
     {:status 404})))

# (def ddb-client-cache @[nil])
# (defn get-ddb-client []
#   (if-let [ddb-client (get ddb-client-cache 0)]
#     ddb-client
#     (let [ddb-client (ddb/make-ddb-client)]
#       (put ddb-client-cache 0 ddb-client)
#       ddb-client)))

(defn create-table [ddb-client]
  (ddb/invoke ddb-client
              {:op :CreateTable
               :request {"TableName" "janet_postings"
                         "AttributeDefinitions"
                         [{"AttributeName" "pk"
                           "AttributeType" "S"}
                          {"AttributeName" "sk"
                           "AttributeType" "S"}
                          # schema less but for completenes
                          # {"AttributeName" "title"
                          #  "AttributeType" "S"}
                          # {"AttributeName" "content"
                          #  "AttributeType" "S"}
                          # {"AttributeName" "userslug"
                          #  "AttributeType" "S"}
                          # {"AttributeName" "username"
                          #  "AttributeType" "S"}
                          # {"AttributeName" "created_at"
                          #  "AttributeType" "S"}
                          # {"AttributeName" "updated_at"
                          #  "AttributeType" "S"}
                          # {"AttributeName" "comment-count"
                          #  "AttributeType" "N"}
                          {"AttributeName" "gsi1pk"
                           "AttributeType" "S"}
                          {"AttributeName" "gsi1sk"
                           "AttributeType" "S"}
                         
                          ]

                         "KeySchema"
                         [{"AttributeName" "pk"
                           "KeyType" "HASH"}
                          {"AttributeName" "sk"
                           "KeyType" "RANGE"}
                          ]

                         "GlobalSecondaryIndexes"
                         [{"IndexName" "gsi1"
                           "KeySchema" [{"AttributeName" "gsi1pk"
                                         "KeyType" "HASH"}
                                        {"AttributeName" "gsi1sk"
                                         "KeyType" "RANGE"}]
                           "Projection" {"ProjectionType" "ALL"}
                           # TODO don't need content & version in projection
                           "ProvisionedThroughput" {"ReadCapacityUnits" 1
                                                    "WriteCapacityUnits" 1}
                           }]
                         "ProvisionedThroughput" {"ReadCapacityUnits" 1
                                                  "WriteCapacityUnits" 1}
                         }}))

(def percent-encoding-mapping
  # https://developer.mozilla.org/en-US/docs/Glossary/percent-encoding
  {"%3A" ":"
   "%2F" "/"
   "%3F" "?"
   "%23" "#"
   "%5B" "["
   "%5D" "]"
   "%40" "@"
   "%21" "!"
   "%24" "$"
   "%26" "&"
   "%27" "'"
   "%28" "("
   "%29" ")"
   "%2A" "*"
   "%2B" "+"
   "%2C" ","
   "%3B" ";"
   "%3D" "="
   "%25" "%"
   "%20" " "})

(defn url-decode [s]
  (def out @"")
  (def slength (length s))
  (var i 0)
  (while (< i slength)
    (let [c (get s i)]
      (case c
        (first "%") (let [percent-encoded-triple (string/slice s i (+ i 3))]
                      (+= i 2)
                      (let [replacement (get percent-encoding-mapping percent-encoded-triple percent-encoded-triple)]
                        (buffer/push-string out replacement)))
        (first "+") (buffer/push-string out " ")
        # default
        c
        (buffer/push-byte out c)))
    (++ i))
  (string out))

(defn before-form-params-handler [request]
  (if (= (get-in request [:headers "content-type"]) "application/x-www-form-urlencoded")
    (let [body (get request :body)]
      (if (string? body)
        (let [params @{}]
          (loop [kv :in (string/split "&" body)]
            (let [[k v] (string/split "=" kv)
                  k (url-decode k)
                  v (url-decode v)]
              (put params k v)))
          (put request :form-params params))
        request))
    request))

(comment
 (before-form-params-handler @{:headers {"content-type" "application/x-www-form-urlencoded"}
                               :body "csrf-token=FORM_CSRF_TOKEN&title=My+first+title%3F*%21"})
 )

(defn before-query-params-handler [request]
  (if-let [query-string (get request :query-string)]
    (let [params @{}]
      (loop [kv :in (string/split "&" query-string)]
        (let [[k v] (string/split "=" kv)]
          (when (and k v)
            (let [k (url-decode k)
                  v (url-decode v)]
              (put params k v)))))
      (put request :query-params params))
    request))


(def cookie-key-box @[nil])
(defn get-cookie-key []
  (if-let [cookie-key (get cookie-key-box 0)]
    cookie-key
    (let [cookie-key (if (os/getenv "JANET_DEV_ENV")
                       "UNSECURE_UNSAFE!"
                       (let [sck (os/getenv "SERVER_COOKIE_KEY")
                             _ (when (or (not sck)
                                         (not= (length sck) 24))
                                 (error "SERVER_COOKIE_KEY needs to be 16 bytes encoded in 24 char length base64 string"))]
                         (-> (crypto/decode-base64 sck)
                             string)))]
      (put cookie-key-box 0 cookie-key)
      cookie-key)))

(defn before-cookie-handler [request]
  (if-let [cookie-header (get-in request [:headers "cookie"])]
    (if (not= cookie-header "")
      (let [cookie-key (get-cookie-key)
            cookies @{}]
        (loop [cookie-string :in (string/split " " cookie-header)]
          (let [[cookie-name cookie-data-string] (string/split "=" cookie-string
                                                               0 # start from
                                                               2 # limit (do not split == within cookie-data-string)
                                                               )
                unsealed-cookie (try (crypto/unseal cookie-key cookie-data-string)
                                     ([err]
                                      (printf "Could not unseal cookie: %p" cookie-name)
                                      nil))]
            (when unsealed-cookie # when unsealing failed this wasn't a cookie we set, ignore it
              (put cookies cookie-name unsealed-cookie))))
        (put request :cookies cookies))
      # cookie header ""
      request)
    # no cookie header present
    request))

(def SESSION_COOKIE_NAME "sess")

(defn before-session-handler [request]
  (if-let [session-cookie-string (get-in request [:cookies SESSION_COOKIE_NAME])]
    (let [session-data (json/decode session-cookie-string
                                    true # keyword keys
                                    )]
      (put request :session session-data)))
  request)

(defn after-session-handler [request response]
  (let [before-session (get request :session)
        after-session (get response :session)]
    (if (and (not before-session)
             (not after-session))
      response ## nothing to do for session

      (let [before-session (or before-session {})
            after-session (or after-session {})
            session (table (splice (kvs before-session)))
            _ (loop [key :keys after-session]
                (let [val (get after-session key)]
                  (if (= val :delete)
                    (put session key nil)
                    (put session key val))))
            cookie-data (if (empty? session)
                          :delete
                          session)]
        (put-in response [:cookies SESSION_COOKIE_NAME] cookie-data)))))

(defn before-csrf-handler [request]
  (let [method (get request :method)
        desired-token (get-in request [:session :csrf-token])
        form-token (or desired-token
                       (crypto/random-token 60))]
    (if (or (= method :get)
            (= method :head)
            (= method :options))
      # not a submit, no need to check
      # put crsf token in data to render in forms
      [(put request :csrf-token form-token)
       nil]

      # assume method was post/a submit
      (let [error-response {:status 500
                            :headers {"Content-Type" "text/plain"}
                            :body "ERROR!"}]
        (if-not desired-token
          [request error-response]
          (let [gotten-csrf-token (or (get-in request [:form-params "csrf-token"])
                                      (when (= method :post)
                                        (get-in request [:headers "x-csrf-Token"])))]
            (if-not gotten-csrf-token
              [request error-response]
              (if (not (crypto/timing-safe= desired-token gotten-csrf-token))
                [request error-response]
                ## csrf-token gotten is valid, include in request to render in form
                [(put request :csrf-token form-token)
                 nil]
                ))))))))

(defn after-csrf-handler [request response]
  (update response :session
          (fn [session]
            (let [# might have set session to struct
                  session (cond
                            (table? session)
                            session
                            (nil? session)
                            @{}
                            :else
                            (table (splice (kvs session))))]
              (update session :csrf-token
                      (fn [token]
                        (or token
                            (get request :csrf-token))))))))

(defn before-flash-handler [request]
  (if-let [flash (get-in request [:session :_flash])]
    (put request :flash flash)
    request))

(defn after-flash-handler [request response]
  (if-let [flash (and (get-in response [:headers "Location"])
                      (get response :flash))]
    (put-in response [:session :_flash] flash)
    (if (get request :flash)
      (put-in response [:session :_flash] :delete)
      response)))

(defn before-user-handler [request]
  (let [self-link (get request :uri) # todo add all query-params except retour!

        retour-link (when-let [retour-param (get-in request [:query-params "retour"])]
                      (when (and (not (string/has-prefix? "http" retour-param))
                                 (not (string/has-prefix? "://" retour-param)))
                        retour-param))

        # use a return link if already set (otherwise you can get a link as /signup?retour=/login on the /login page)
        login-signup-goto-link (or retour-link
                                   self-link)

        # never do retour to /login or /signup
        # (this is more relevant when self-link is no longer hard-coded to /)
        login-signup-goto-link (if (or (string/has-prefix? "/login" login-signup-goto-link)
                                       (string/has-prefix? "/signup" login-signup-goto-link))
                                 nil
                                 login-signup-goto-link)

        user (get-in request [:session :auth :user])

        retour-part (when login-signup-goto-link
                      (string "?retour=" login-signup-goto-link))]
    (merge-into request
                {:auth.links/login (string "/login" retour-part)
                 :auth.links/signup (string "/signup" retour-part)
                 :header.links/index "/"
                 :auth.links/retour retour-link # absent when nil
                 }
                (if (and user
                         (auth/assert-user user))
                  {:auth.user/userslug (get user :userslug)
                   :auth.user/username (get user :username)
                   :auth.user/roles (get user :roles)
                   :auth.links/profile (string "/profile/" (get user :userslug))
                   :auth.links/logout (string "/logout?retour=" self-link)}
                  {}))
    ))

(defn before-handlers [request]
  (let [response nil
        request (before-query-params-handler request)
        request (before-form-params-handler request)
        request (before-cookie-handler request)
        request (before-session-handler request)
        request (before-flash-handler request)
        request (before-user-handler request)
        [request response] (before-csrf-handler request)]
    [request response]))

(defn cookie-postfix-settings []
  (string ";Path=/;HttpOnly;SameSite=Lax"
          (if (os/getenv "JANET_DEV_ENV")
            ""
            ";Secure")))



(defn after-cookie-handler [request response]
  (if-let [cookies (get response :cookies)]
    (let [cookie-key (get-cookie-key)
          cookie-strings (seq [cookie-name :keys cookies]
                              (let [cookie-data (get cookies cookie-name)]
                                (if (= cookie-data :delete)
                                  ## delete cookie with negative max-age
                                  (string cookie-name "=DELETE_COOKIE;Max-Age=-1" (cookie-postfix-settings))
                                  ## set cookie with data
                                  (let [cookie-data-string (-> (json/encode cookie-data)
                                                               string)]
                                    (string cookie-name "=" (crypto/seal cookie-key cookie-data-string) (cookie-postfix-settings))))))
          cookie-header (string/join cookie-strings " ")]
      (put-in response [:headers "Set-Cookie"] cookie-header))
    response))

(defn after-handlers [request response]
  (let [response (if (table? response)
                   response
                   # from struct
                   (table (splice (kvs response))))
        # turn headers into table
        response (update response :headers
                         (fn [existing-headers]
                           (cond
                             (nil? existing-headers)
                             @{}
                             (table? existing-headers)
                             existing-headers
                             :else
                              (table (splice (kvs existing-headers))))))
        response (after-csrf-handler request response)
        response (after-flash-handler request response)
        response (after-session-handler request response)
        response (after-cookie-handler request response)]
    response))

(defn service [request]
  (def [request response] (before-handlers request))
  (if response
    response             # shortcut if before-handlers made a response

    (let [method (get request :method)
          uri (get request :uri)
          response (cond
                     #
                     # TOGGLE with env var on prod
                     #
                     # (and (= (get request :method) :get)
                     #      (= (get request :uri) "/bootstrap")
                     #      (os/getenv "POSTINGS_BOOTSTRAP_ENABLED"))
                     # (let [ddb-result (create-table ddb-client)
                     #       res {:status 200
                     #            :headers {"Content-Type" "application/json"}
                     #            :body (json/encode ddb-result)}]
                     #   res)

                     (and (= method :get)
                          (= uri "/favicon.ico"))
                     {:status 404}

                     (and (= method :get)
                          (= uri "/login"))
                     (auth/login request)

                     (and (= method :post)
                          (= uri "/login"))
                     (auth/login-submit request)

                     (and (= method :post)
                          (= uri "/guestlogin"))
                     (auth/guest-login-submit request)

                     (and (= method :get)
                          (= uri "/signup"))
                     (auth/signup request)

                     (and (= method :post)
                          (= uri "/signup"))
                     (auth/signup-submit request)

                     (and (= method :post)
                          (= uri "/logout"))
                     (auth/logout-submit request)

                     (and (= method :get)
                          (= uri "/"))
                     (posts/posts-index request)

                     (and (= method :get)
                          (= uri "/posts/new"))
                     (or (auth/respond-require-role request :user)
                         (posts/posts-new request))

                     (and (= method :post)
                          (= uri "/posts/new"))
                     (or (auth/respond-require-role request :user)
                         (posts/posts-new-submit request))

                     (and (= method :get)
                          (string/has-prefix? "/posts/" uri))
                     (if (string/has-suffix? "/reply" uri)
                       (or (auth/respond-require-role request :user)
                           (posts/posts-reply request))
                       (posts/posts-show request))

                     (and (= method :post)
                          (string/has-prefix? "/posts/" uri)
                          (string/has-suffix? "/reply" uri))
                     (or (auth/respond-require-role request :user)
                         (posts/posts-reply-submit request))

                     (and (= method :get)
                          (string/has-prefix? "/profile/" uri))
                     (profile/profile-show request)

                     (and (= method :get)
                          (string/has-prefix? "/public" uri))
                     (file-handler request "public")

                     :else
                     {:status 404}
                     )]
      (after-handlers request response))))


(defn lambda-json->request [in]
  # works for aws gateway lambda api version 2
  (let [lj (json/decode in)

        # name=val; name1=val2
        cookies (when-let [c (get lj "cookies")]
                  (string/join c "; "))]
    # MAYBE include all lj fields in req?
    @{:headers (let [headers (get lj "headers" {})
                     t @{}]
                 (when cookies
                   (put t "cookie" cookies))
                 (loop [header-key :keys headers]
                   (put t (string/ascii-lower header-key) (get headers header-key)))
                 t)
      :uri (get lj "rawPath")

      :body (when-let [s (get lj "body")]
              (if (get lj "isBase64Encoded")
                (-> (crypto/decode-base64 s)
                    string)
                s))
      :scheme (get {"http" :http
                    "https" :https}
                   (get lj "scheme"))
      :method (get {"GET" :get
                    "POST" :post}
                   (get-in lj
                           ["requestContext"
                            "http"
                            "method"]))
      :query-string (get lj "rawQueryString")}))

(defn response->lambda-json [response]
  (if-let [file-buf (get response :file-buf)]
    (let [file-path (get response :file-path "")
          content-type-header (cond (string/has-suffix? ".css" file-path)
                                    "text/css; charset=utf-8"
                                    (string/has-suffix? ".js" file-path)
                                    "text/javascript; charset=utf-8"
                                    :else
                                    "text/octet-stream")
          content-length-header (length file-buf)

          headers (merge {"Content-Type" content-type-header
                          "Content-Length" content-length-header}
                         (get response :headers {}))

          body-base64 (crypto/encode-base64 file-buf)

          res {:isBase64Encoded true
               :statusCode (get response :status 200)
               :headers headers
               :body body-base64
               }]
      (json/encode res))

    # non file responses
    (let [body (get response :body)

          headers (get response :headers {})
          headers (if body
                    (merge-into @{"Content-Length" (string (length body))}
                                headers)
                    headers)

          res-l {:isBase64Encoded false
                 :statusCode (get response :status 200)
                 :headers headers
                 :body body # if body nil, will be absent in res-l (needed because api gateway chokes on ':body nil')
                }]
      (json/encode res-l))))

(defn service-handler [request]
  (let [request (lambda-json->request request)
        response (service request)]
    (response->lambda-json response)))

(defn get-runtime-api []
  (or (os/getenv "AWS_LAMBDA_RUNTIME_API")
      "localhost:8080"))

(defn main [& args]
  (def runtime-api (get-runtime-api))
  (print "Running service for runtime-api: " runtime-api)
  (print "Init random")
  (math/seedrandom (os/cryptorand 8))
  (forever
   (let [request (http/get (string "http://" runtime-api "/2018-06-01/runtime/invocation/next"))
         request-headers (get request :headers)
         lambda-runtime-aws-request-id (or (get request-headers "Lambda-Runtime-Aws-Request-Id")
                                           (get request-headers "lambda-runtime-aws-request-id"))]
     (try
       (let [ # the api-gateway request as lambda event in json as string
             request-body (get request :body)
             response (service-handler request-body)

             #response (string response)
             response (buffer/trim response)]
         (http/request "POST"
                       (string "http://" runtime-api "/2018-06-01/runtime/invocation/" lambda-runtime-aws-request-id "/response")
                       {:headers {"Content-Type" "application/json"
                                  "Content-Length" (string (length response))}
                        :body response}))
       ([err fib]
        (print "Error in main service f" err)
        (let [b @""]
          (with-dyns [:err b]
            (debug/stacktrace fib err))
          (print b))
        (let [response "{\"error\": \"main-service\"}"]
          (http/request "POST"
                        (string "http://" runtime-api "/2018-06-01/runtime/invocation/" lambda-runtime-aws-request-id "/error")
                        {:headers {"Content-Type" "application/json"
                                   "Content-Length" (string (length response))}
                         :body response}))))))
  )

(comment

 (def ddb-client (ddb/make-ddb-client))


 (ddb/invoke ddb-client
             {:op :DescribeTable
              :request {"TableName" "janet_postings"}})

 (ddb/invoke ddb-client
             {:op :DeleteTable
              :request {"TableName" "janet_postings"}})

 (ddb/invoke ddb-client
             {:op :ListTables
              :request {}})
 (create-table ddb-client)

 (create-table ddb-client)

 (import ./data)
 (create-table (data/get-client))
)
