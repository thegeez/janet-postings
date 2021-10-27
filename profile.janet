(import ./temple/temple :as temple)
(import ./data)

(def profile-show-template (temple/make-template-fn "templates/profile.html"))

(defn profile-show [request]
  (let [userslug (string/slice (get request :uri) (length "/profile/"))
        profile (data/get-profile userslug)]
    (if (get profile :error)
      {:status 404}
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (profile-show-template (merge request
                                           {:post.links/index "/"
                                            :post.links/new "/posts/new"
                                            :username (get profile :username)
                                            :created-at-date (get profile :created-at-date)
                                            :created-at-ago (get profile :created-at-ago)}))})))

(comment
 (profile-show {:uri "/profile/helloworld8"})
 )
