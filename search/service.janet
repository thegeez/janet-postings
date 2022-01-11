(import /lib/http)
(import json)
(import ./search :as search)

(defn service [event]
  (cond
    (get event "Records")        # inputs from dynamo db stream
    (search/handle-import event)

    (get event "Search")
    (search/search event)

    (get event "report")
    (search/report)

    (get event "bootstrap")
    (search/bootstrap)

    (get event "destroy_dangerous")
    (search/destroy)

    :else
     (do
       (eprintf "Unknown event with keys %p" (keys event))
       {:error "Unknown event for search-service"})
    ))

(defn service-handler [request]
  (let [event (json/decode request)
        result (service event)]
    (json/encode result)))

(defn get-runtime-api []
  (or (os/getenv "AWS_LAMBDA_RUNTIME_API")
      "localhost:8081"))

(defn main [& args]
  (def runtime-api (get-runtime-api))
  (print "Running service for runtime-api: " runtime-api)
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
        (eprint "Error in search service f" err)
        (let [b @""]
          (with-dyns [:err b]
            (debug/stacktrace fib err))
          (eprint b))
        (let [response "{\"error\": \"search-service\"}"]
          (http/request "POST"
                        (string "http://" runtime-api "/2018-06-01/runtime/invocation/" lambda-runtime-aws-request-id "/error")
                        {:headers {"Content-Type" "application/json"
                                   "Content-Length" (string (length response))}
                         :body response}))))))
  )
