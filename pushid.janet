# h/t https://gist.github.com/mikelehen/3596a30bd69384624c11

(def push-chars "0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
(assert (= (length push-chars) 64))
(assert (= (length (distinct push-chars)) 64))
(assert (= (-> (buffer/push-string @"" push-chars)
               sort
               string)
           push-chars))

# note: does not handle same time collisions!
(defn generate-push-id []
  (var now (math/round (* 1000 (os/clock))))
  (let [pushid-chars (buffer/new 20)
        # timestamp chars
        _ (loop [i :down-to [7 0]]
            (let [chunk (% now 64)
                  timestamp-char (get push-chars chunk)]
              (put pushid-chars i timestamp-char)
              (if (zero? now)
                (break)
                (set now (math/floor (/ now 64)))
                )))
        # rand chars
        _ (loop [i :range [0 12]]
            (let [rand-idx (math/floor (* (math/random) 64))
                  rand-char (get push-chars rand-idx)]
              (buffer/push-byte pushid-chars rand-char)))
        id (string pushid-chars)]
    (assert (= 20 (length id))
            (string "Length should be 20 " id " "(length id)))
    id
    ))

(comment
 (generate-push-id)
 )
