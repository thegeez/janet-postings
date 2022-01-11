(import ./search/search)
(import json)
(import ./aws_api/aws_api :as aws-api)

(defn search-dev [q limit offset]
  (-> (search/search {"query" q
                      "limit" limit
                      "offset" offset})
      # to make dev and live results look the same
      (json/encode)
      (json/decode)))

(def function-arn-box @[nil])

(defn get-function-arn []
  (if-let [function-arn (get function-arn-box 0)]
    function-arn
    (let [function-arn (os/getenv "SEARCH_LAMBDA_FUNCTION_ARN")]
      (assert function-arn "Search backend not configured")
      (put function-arn-box 0 function-arn)
      function-arn)))

(defn search-lambda [q limit offset]
  (let [lambda-client (aws-api/make-lambda-client)

        function-arn (get-function-arn)

        lambda-request (-> (json/encode {"Search" true
                                         "query" q
                                         "limit" limit
                                         "offset" offset})
                           string)]
    (try
      (-> (aws-api/invoke lambda-client
                          {:op :Invoke
                           :uri (string "/2015-03-31/functions/" function-arn "/invocations")
                           :request lambda-request})
          (json/decode))
      ([err fib]
       (eprintf "Could not execute search" err)

       {:error (string/format "Could not execute search" err)}))))

(defn search [q limit offset]
  (if (os/getenv "JANET_DEV_ENV")
    (search-dev q limit offset)
    (search-lambda q limit offset)))

