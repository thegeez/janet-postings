(import ./pushid :as pushid)
(import ./data)
(import ./data_search :as data-search)
(import jalmer)

(def posts-index-template (jalmer/make-template-fn "template/index.html"))

(def posts-new-template (jalmer/make-template-fn "template/new.html"))
(def posts-show-template (jalmer/make-template-fn "template/show.html"))
(def posts-reply-template (jalmer/make-template-fn "template/reply.html"))

(def posts-search-template (jalmer/make-template-fn "template/search.html"))

(defn posts-index [request]
  (let [## before/after is "20211023T200847Z_0MmilH44Uu3YgNPVII_f"
        before (get-in request [:query-params "before"])
        after (get-in request [:query-params "after"])
        limit (get-in request [:query-params "limit"] 3)
        posts (data/get-posts before after limit)
        posts (seq [post :in posts]
                   (merge-into @{:post.links/show (string "/posts/" (get post :slug))
                                 :post.links/user-profile (string "/profile/" (get post :userslug))}
                               post))

        after-link (when-let [last-post (last posts)]
                     (string "/?after=" (get last-post :cdate) "_" (get last-post :slug)))
        before-link (when-let [first-post (first posts)]
                      (string "/?before=" (get first-post :cdate) "_" (get first-post :slug)))]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (posts-index-template (merge request
                                        {:posts.links/new "/posts/new"
                                         :posts.links/recent "/posts/recent"
                                         :posts.links/before before-link
                                         :posts.links/after after-link
                                         :posts posts}))
    }))

(defn posts-new [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (posts-new-template (merge request
                                    {:post.links/index "/"
                                     :post.links/action "/posts/new"
                                     :post.links/back "/"
                                    }))
  })

(defn validate-post [title text]
  {:title (when (<= (length title) 10)
            "The title needs to be at least 10 characters long")
   :text (when (<= (length text) 10)
           "The text must be at least 10 characters long")})

(defn posts-new-submit [request]
  (let [{"title" title
         "text" text} (get request :form-params)
        errors (validate-post title text)]
    (if (not (empty? errors))
      {:status 200
       :headers {"Content-Type" "text/html"}
       #:body "<h1>Hello world! from posts!!!12344567awesome!!22!123</h1>"
       :body (posts-new-template (merge request
                                        {:post.links/index "/"
                                         :post.links/action "/posts/new"
                                         :post.links/back "/"
                                         :errors errors
                                         :title title
                                         :text text}))}

      (let [slug (pushid/generate-push-id)

            userslug (get request :auth.user/userslug)
            username (get request :auth.user/username)

            now (os/date)

            put-res (data/put-post {:slug slug
                                    :username username
                                    :userslug userslug
                                    :created-at now
                                    :title title
                                    :text text})]
        (if (get put-res :error)
          {:status 200
           :headers {"Content-Type" "text/html"}
           :body (posts-new-template (merge request
                                            {:post.links/index "/"
                                             :post.links/action "/posts/new"
                                             :post.links/back "/"
                                             :errors {:db "Something went wrong while submitting, please try again"}
                                             :title title
                                             :text text}))
          }
          # db submit success
          {:status 303
           :flash {:info (string "Created post with slug " slug)}
           :headers {"Location" (string "/posts/" slug)}})))))

(defn posts-show [request]
  (let [post-slug (string/slice (get request :uri) (length "/posts/"))
        show-link (string "/posts/" post-slug)

        post (data/get-post+comments post-slug)]
    (if post
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (posts-show-template (merge request
                                         {:post.links/show (string "/posts/" (get post :slug))
                                          :post.links/index "/"
                                          :post.links/new "/posts/new"
                                          :post.links/user-profile (string "/profile/" (get post :userslug))
                                          :post.links/reply (when (get request :auth.user/userslug) # only present for logged in users
                                                              (string "/posts/" (get post :slug) "/reply"))}
                                         (update post :comments
                                            (fn [comments]
                                              (seq [comment :in comments]
                                                   (merge-into @{:comment.links/user-profile (string "/profile/" (get comment :userslug))
                                                                 :comment.links/show (string "/posts/" (get post :slug) "#" (get comment :comment-order-id))
                                                                 :comment.links/reply (when (get request :auth.user/userslug) # only present for logged in users
                                                                                        (string "/posts/" (get post :slug) "/reply?reply=" (get comment :comment-order-id))) }
                                                               comment))))
                                         ))}
      # or 303 to index with flash not found
      {:status 404})))

(defn posts-reply [request]
  (let [post-slug (string/slice (get request :uri) (length "/posts/") (- -1 (length "/reply")))

        comment-parent-id (or (get-in request [:query-params "reply"])
                              "POST")

        show-link (string "/posts/" post-slug)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (posts-reply-template (merge request
                                        {:comment.links/post show-link
                                         :comment.links/back show-link
                                         :comment.links/index "/"
                                         :comment.links/action (get request :uri)
                                         :comment/reply-to comment-parent-id
                                        }))}))

(defn validate-comment [text]
  {:text (when (< (length text) 10)
           "The text must be at least 10 characters long")})

(defn posts-reply-submit [request]
  (let [post-slug (string/slice (get request :uri) (length "/posts/") (- -1 (length "/reply")))
        show-link (string "/posts/" post-slug)

        reply-to (get-in request [:form-params "reply-to"])
        _ (assert reply-to "form did not contain reply-to")
        text (get-in request [:form-params "text"] "")

        parent-comment-order-id (string/replace "-" "#" reply-to)

        errors (validate-comment text)]
    (if (not (empty? errors))
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (posts-reply-template (merge request
                                          {:comment.links/post show-link
                                           :comment.links/back show-link
                                           :comment.links/index "/"
                                           :comment.links/action (get request :uri)
                                           :comment/reply-to parent-comment-order-id
                                           :errors errors
                                           :text text
                                           }))}
      (let [parent-comment-order-id (if (= parent-comment-order-id "POST")
                                      "B" # start of a new comment thread path
                                      (string "B#" parent-comment-order-id))
            comment-order-id (string parent-comment-order-id "#" (pushid/generate-push-id))

            now (os/date)

            comment {:post-slug post-slug
                     :comment-order-id comment-order-id
                     :userslug (get request :auth.user/userslug)
                     :username (get request :auth.user/username)
                     :created-at now
                     :text text}
            put-res (data/put-comment comment)]
        (if (get put-res :error)
          {:status 200
           :headers {"Content-Type" "text/html"}
           :body (posts-reply-template (merge request
                                              {:comment.links/post show-link
                                               :comment.links/back show-link
                                               :comment.links/index "/"
                                               :comment.links/action (get request :uri)
                                               :comment/reply-to parent-comment-order-id
                                               :errors errors
                                               :text text
                                               }))}
          # db submit success
          {:status 303
           :flash {:info "Created comment"}
           :headers {"Location" show-link}})))))

(defn highlight-pieces [s]
  # "__b__Third__/b__ post!" -> @["" :b "Third" :bend " post!"]
  (mapcat
   (fn [item]
     (if (string? item)
       (interpose :endb
                  (string/split "__/b__" item))
       item))
   (interpose :b
              (string/split "__b__" s))))

(defn posts-search [request]
  (let [q (get-in request [:query-params "q"])]
    (if (or (not q)
            (< (length q) 3))
      {:status 303
       :flash {:info "Search needs a proper query"}
       :headers {"Location" "/"}}

      (let [limit (let [limit-str (get-in request [:query-params "limit"] "10")
                        limit (try (scan-number limit-str)
                                   ([err]
                                    10))]
                    (max 2 (min limit 25)))

            offset (let [offset-str (get-in request [:query-params "offset"] "0")
                         offset (try (scan-number offset-str)
                                     ([err]
                                      0))]
                     (max 0 offset))

            results (data-search/search q
                                        limit  # limit
                                        offset # offset
                                        )]
        (if-let [error-msg (or (get results :error)
                               (get results "error"))]
          {:status 400
           :body error-msg}

          (let [items (seq [item :in (data/search-items->posts&comments (get results "items"))]
                           (let [item (table (splice (kvs item)))]
                             (if (= (get item :type) "POST")
                               (-> item
                                   (put :post.links/show (string "/posts/" (get item :slug)))
                                   (put :post.links/user-profile (string "/profile/" (get item :userslug)))
                                   (put :title_pieces (highlight-pieces (get item :title_highlight)))
                                   (put :text_pieces (highlight-pieces (get item :text_highlight))))

                               (-> item
                                   (put :comment.links/show (string "/posts/" (get item :slug)))
                                   (put :comment.links/user-profile (string "/profile/" (get item :userslug)))
                                   (put :text_pieces (highlight-pieces (get item :text_highlight)))))))
                result-count (get results "total-count")
                previous-link (when (<= 0 (- offset limit))
                                (string/format "/search?q=%s&limit=%d&offset=%d" q limit (- offset limit)))
                next-link (when (< (+ offset limit) result-count)
                            (string/format "/search?q=%s&limit=%d&offset=%d" q limit (+ offset limit)))]
            {:status 200
             :headers {"Content-Type" "text/html"}
             :body (posts-search-template (merge request
                                                 {:search/query q
                                                  :search/items items
                                                  :search/result-count result-count
                                                  :search.links/previous previous-link
                                                  :search.links/next next-link
                                                 }))}))))))

