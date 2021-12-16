(import ./data)
(import ./lib/encdecsignhash)
(import ./crypto)
(import ./selmer :as selmer)

(defn assert-user [user]
  (assert (get user :username))
  (assert (get user :userslug))
  (assert (get user :roles))
  user)

(def auth-login-template (selmer/make-template-fn "template/login.html"))
(def auth-signup-template (selmer/make-template-fn "template/signup.html"))


(def allowed-userslug-chars-set
  (let [t @{}]
    (loop [c :in "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-"]
      (put t c true))
    t))

(defn username->userslug [username]
  (let [b @""]
    (loop [c :in username
           :when (get allowed-userslug-chars-set c)]
      (buffer/push-byte b c))
    (-> b
        string
        string/ascii-lower)))

(defn validate-password [password-one password-two]
  (merge {:password-one (when (empty? (string/trim password-one))
                          "Please fill in a password")
          :password-two (when (empty? (string/trim password-two))
                          "Please fill in a password")}
         {:password-one (when (< (length password-one) 5)
                          "The password needs to be at least 5 characters")
          :password-two (when (not (= password-one password-two))
                          "The password comfirmation does not match the password")}))

(defn validate-username [username]
  {:username (when (< (length username) 5)
               "The username needs to be at least 5 characters")})

(defn validate-userslug [userslug]
  {:username (when (< (length userslug) 5)
               "The username is not allowed")})


(defn encrypt-password [password]
  (let [b @""]
    (encdecsignhash/crypt-into-buffer password b)
    (string b)))

(defn check-password [password password-encrypted]
  (let [check-encrypted (let [b @""]
                          (encdecsignhash/crypt-with-salt-into-buffer password
                                                                      password-encrypted # encrypted pw has salt as a its prefix to reuse
                                                                      b)
                          (string b))]
    (if (crypto/timing-safe= check-encrypted password-encrypted)
      true
      false)))

(defn check-account [userslug password]
  (let [res (data/get-auth userslug)

        error-response {:errors {:username "Could not login with this username and password"}}]
    (if (get res :error)
      error-response

      (if (not (check-password password (get res :password-encrypted)))
        error-response

        {:user {:userslug (get res :userslug)
                :username (get res :username)
                :roles (get res :roles)}}))))

(defn create-account [userslug username password]
  (let [roles {:user true}

        password-encrypted (encrypt-password password)

        auth {:userslug userslug
              :username username
              :password-encrypted password-encrypted
              :roles roles}

        res (data/put-auth auth)]
    (if (get res :error)
      (do
        (eprintf "Err: Create account err res: %p" res)
        {:errors {:username "This username is not available"}})
      {:user {:userslug userslug
              :username username
              :roles roles}})))

(defn login [request]
  (let [retour (get request :auth.links/retour)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (auth-login-template (merge request
                                       {:login.links/action (string "/login" (when retour
                                                                               (string "?retour=" retour)))
                                        :login.links/cancel (or retour "/")
                                        :links/guest-login-action (string "/guestlogin" (when retour
                                                                                          (string "?retour=" retour)))}
                                       ))}))

(defn login-submit [request]
  (let [{"username" username
         "password" password} (get request :form-params)

        # can be checked before calling db
        errors (merge (validate-username username)
                      {:password (when (empty? password)
                                   "Please fill in your password")})
        retour (get request :auth.links/retour)
        action-link (string "/login" (when retour
                                       (string "?retour=" retour)))
        cancel-link (or retour "/")
        guest-link (string "/guestlogin" (when retour
                                           (string "?retour=" retour)))]
    (if (not (empty? errors))
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (auth-login-template (merge request
                                         {:login.links/action action-link
                                          :login.links/cancel cancel-link
                                          :links/guest-login-action guest-link
                                          :errors errors
                                          :username username}
                                         ))}
      # check account in db
      (let [userslug (username->userslug username)
            res (check-account userslug password)]
        (if-let [errors (get res :errors)]
          {:status 200
           :headers {"Content-Type" "text/html"}
           :body (auth-login-template (merge request
                                             {:login.links/action action-link
                                              :login.links/cancel cancel-link
                                              :links/guest-login-action guest-link
                                              :errors errors
                                              :username username}
                                             ))}
          # auth succes
          (let [{:user user} res
                _ (assert-user user)
                goto (or retour "/")]
            {:status 303
             :session {:auth {:user user}}
             :flash {:info "You are now logged in"}
             :headers {"Location" goto}})
          )))))

(defn signup [request]
  (let [retour (get request :auth.links/retour)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (auth-signup-template (merge request
                                        {:signup.links/action (string "/signup" (when retour
                                                                                  (string "?retour=" retour)))
                                         :signup.links/cancel (or retour "/")
                                         :links/guest-login-action (string "/guestlogin" (when retour
                                                                                           (string "?retour=" retour)))}
                                        ))}))

(defn signup-submit [request]
  (let [{"username" username
         "password-one" password-one
         "password-two" password-two} (get request :form-params)

        userslug (username->userslug username)

        errors (merge (validate-password password-one password-two)
                      (validate-username username)
                      (validate-userslug userslug))
        retour (get request :auth.links/retour)]
    (if (not (empty? errors))
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (auth-signup-template (merge request
                                          {:signup.links/action (string "/signup" (when retour
                                                                                    (string "?retour=" retour)))
                                           :signup.links/cancel (or retour "/")
                                           :links/guest-login-action (string "/guestlogin"
                                                                             (when retour
                                                                               (string "?retour=" retour)))
                                           :errors errors
                                           :username username}
                                          ))
       }

      (let [## password has been validated above
            password password-one

            ## check account may fail with errors
            res (create-account userslug
                                username
                                password)]
        (if-let [errors (get res :errors)]
          {:status 200
           :headers {"Content-Type" "text/html"}
           :body (auth-signup-template (merge request
                                              {:signup.links/action (string "/signup" (when retour
                                                                                        (string "?retour=" retour)))
                                               :signup.links/cancel (or retour "/")
                                               :links/guest-login-action (string "/guestlogin"
                                                                                 (when retour
                                                                                   (string "?retour=" retour)))
                                               :errors errors
                                               :username username}
                                              ))}
          # account create success
          (let [user (get res :user)
                goto (or retour
                         "/")]
            {:status 303
             :session {:auth {:user (do (assert-user user)
                                        user)}}
             :flash {:info "You are now logged in"}
             :headers {"Location" goto}}))))))


(defn guest-login-submit [request]
  (let [user-id (string (splice (seq [i :range [0 12]]
                                     (math/round (* 10 (math/random))))))

        username (string "Guest " user-id)

        userslug (username->userslug username)

        password username

        res (create-account userslug
                            username
                            password)

        retour (get request :auth.links/retour)]
    (if-let [errors (get res :errors)]
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (auth-login-template (merge request
                                         {:login.links/action (string "/login"
                                                                      (when retour
                                                                        (string "?retour=" retour)))
                                          :login.links/cancel (or retour "/")
                                          :links/guest-login-action (string "/guestlogin"
                                                                            (when retour
                                                                              (string "?retour=" retour)))
                                          :errors errors}
                                         ))}
      # account create success
      (let [user (get res :user)
            goto (or retour
                     "/")]
        {:status 303
         :session {:auth {:user (do (assert-user user)
                                    user)}}
         :flash {:info "You are now logged in"}
         :headers {"Location" goto}}))))


(defn logout-submit [request]
  (let [goto (or (get request :auth.links/retour)
                 "/")]
    {:status 303
     :session {:auth :delete}
     :flash {:info "You are now logged out"}
     :headers {"Location" goto}}))


(defn respond-require-role [request role]
  # return nil if user has role, else redirect to login/singup
  (let [user-roles (get request :auth.user/roles)]
    (if (and user-roles
             (get user-roles role))
      nil ## user with role is passed through to next handler

      (let [## authorization is not sufficient
            goto-link (get request :auth.links/login)]
        {:status 303
         :flash {:info "You do not have the proper authorization for that action"}
         :headers {"Location" goto-link}})
      )))

(comment
 (encrypt-password "abcde")
 (let [password "abcde"
       b @""]
   (encdecsignhash/crypt-into-buffer password b)
   (string b))
 )
