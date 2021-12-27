(import ./data)
(import jalmer)

(def profile-show-template (jalmer/make-template-fn "template/profile.html"))

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
