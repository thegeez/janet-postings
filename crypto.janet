(import /lib/encdecsignhash :as encdecsignhash)

(defn encode-base64 [in-buffer]
  # 3 x 8 bits = 4 x 6 bits
  (let [out-length (math/ceil (* (/ (length in-buffer) 3) 4))
        out (buffer/new out-length)
        bin-to-c (fn [bin]
                   (cond
                     (<= 0 bin 25) # 0-25 to A-Z (65-90)
                     (+ bin 65)
                     (<= 26 bin 51) # 26-51 a-z (97-122)
                     (+ bin 71)
                     (<= 52 bin 61) # 52-61 0-9 (48-57)
                     (- bin 4)
                     (= bin 62)
                     43         # +
                     (= bin 63)
                     47         # /
                     ))]
    (loop [i :range [0 (length in-buffer)
                     3          # step
                     ]]
      (let [b0 (get in-buffer i)
            b1 (get in-buffer (+ i 1))
            b2 (get in-buffer (+ i 2))

            c0 (-> (brshift b0 2)
                   bin-to-c)
            c1 (let [b1 (or b1 0x00)]
                 (-> (bor (-> (band 2r00000011 b0)
                              (blshift 4))
                          (brshift b1 4))
                     bin-to-c))
            c2 (if b1
                 (let [b2 (or b2 0x00)]
                   (-> (bor (-> (band 2r00001111 b1)
                                (blshift 2))
                            (brshift b2 6))
                       bin-to-c))
                 61             # =
                 )
            c3 (if b2
                 (-> (band 2r00111111
                           b2)
                     bin-to-c)
                 61             # =
                 )]
        (buffer/push-byte out c0)
        (buffer/push-byte out c1)
        (buffer/push-byte out c2)
        (buffer/push-byte out c3)))
    out))


# [(first "a") #  97
#  (first "z") # 122
#  (first "A") #  65
#  (first "Z") #  90
#  (first "0") #  48
#  (first "9") #  57
#  (first "+") #  43
#  (first "/") #  47
#  (first "=")]#  padding
(defn decode-base64 [in-string]
  (let [in-string-length (length in-string)
        _ (when (not (zero? (% in-string-length 4)))
            (error "Not a base64 string as length not a 4 multiple"))
        out (buffer/new (* in-string-length 2))
        c-to-bin (fn [c]
                   (cond
                     (<= 65 c 90) # A-Z
                     (- c 65)
                     (<= 97 c 122) # a-z
                     (+ (- c 97) 26)
                     (<= 48 c 57) # 0-9
                     (+ (- c 48) 52)
                     (= c 43)   # +
                     62
                     (= c 47)   # /
                     63
                     (= c 61)   # =
                     nil        # padding
                     :else
                     (error (string "Non base64 char: " c))
                     ))]
    (loop [i :range [0 in-string-length
                     4          # 4 x 6 = 3 x 8
                     ]]
      (let [c0 (get in-string i)
            c1 (get in-string (+ i 1))
            c2 (get in-string (+ i 2))
            c3 (get in-string (+ i 3))
            b0 (c-to-bin c0)
            b1 (c-to-bin c1)
            b2 (c-to-bin c2)
            b3 (c-to-bin c3)]
        (when (and b0 b1)
          (let [out-byte0 (bor (-> (band 2r00111111 b0)
                                   (blshift 2))
                               (brshift b1 4))]
            (buffer/push-byte out out-byte0)))
        (when (and b1 b2)       # do not output if padding
          (let [out-byte1 (bor (-> (band 2r00001111 b1)
                                   (blshift 4))
                               (brshift b2 2))]
            (buffer/push-byte out out-byte1)))
        (when (and b2 b3)
          (let [out-byte2 (bor (-> (band 2r00000011 b2)
                                   (blshift 6))
                               b3)]
            (buffer/push-byte out out-byte2)))))
    # due to padding we may have allocated too much room
    (buffer/trim out)))

(comment
 "Hello world"
 (decode-base64 "SGVsbG8gd29ybGQ=")
 (-> "Hello world"
     encode-base64
     decode-base64
     )
 )

(defn random-token [n]
  (let [buf (buffer/new n)]
    (encdecsignhash/random-bytes-into-buffer n buf)
    (-> (encode-base64 buf)
        string)))


(defn hmac-sha256 [key data]
   (let [b @""]
     (encdecsignhash/hmac-sha256-into-buffer key data b)
     (string b)))

(defn aes-cbc-encrypt [key data]
  (let [b @""]
     (encdecsignhash/aes-cbc-encrypt-into-buffer key data (length data) b)
     (-> (encode-base64 b)
         string)))

(defn aes-cbc-decrypt [key data]
  (let [data (decode-base64 data)
        iv (string/slice data 0 16)
        data-part (string/slice data 16)
        b @""]
     (encdecsignhash/aes-cbc-decrypt-into-buffer key iv data-part (length data-part) b)
     (string b)))


(defn timing-safe= [a b]
  # check each char for equals, do not shortcut early
  (and (= (length a) (length b))
       (zero?
        (reduce2
          bor
          (map bxor a b)))))


(defn unseal [cookie-key cookie-string]
  (let [# NOTE cookie-string may be url-encoded, may need to add url-decode
        [data-base64 gotten-mac-base64] (string/split "--" cookie-string)
        data (-> (decode-base64 data-base64)
                 string)
        gotten-mac (-> (decode-base64 gotten-mac-base64)
                       string)
        calculated-mac (hmac-sha256 cookie-key data)
       ]
    (when (timing-safe= gotten-mac calculated-mac)
      (aes-cbc-decrypt cookie-key data)
      )))

(defn seal [cookie-key cookie-string]
  (let [data-enc (aes-cbc-encrypt cookie-key cookie-string)
        calculated-mac (-> (hmac-sha256 cookie-key data-enc)
                           encode-base64
                           string)
        data-base64 (-> data-enc
                        encode-base64
                        string)]
    (string data-base64 "--" calculated-mac)))
