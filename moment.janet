(def year-seconds (* 365 24 60 60))
(def month-seconds (* 30 24 60 60))
(def day-seconds (* 24 60 60))
(def hour-seconds (* 60 60))
(def minute-seconds 60)

(defn amz-time-to-seconds [amz-time]
  (let [year (-> (string/slice amz-time 0 4)
                 scan-number)
        month (-> (string/slice amz-time 4 6)
                  scan-number)
        day (-> (string/slice amz-time 6 8)
                scan-number)
        hour (-> (string/slice amz-time 9 11)
                 scan-number)
        minute (-> (string/slice amz-time 11 13)
                    scan-number)
        second (-> (string/slice amz-time 6 8)
                   scan-number)]
    (os/mktime {:year year
                :month (dec month)
                :month-day (dec day)
                :hours hour
                :minutes minute
                :seconds second})))

(defn time-ago-str [from-secs to-secs]
  # this is not quite precise, can be wrong by a day or more
  (let [diff-secs (- to-secs from-secs)]
    (cond
      (< year-seconds diff-secs)
      (let [n-years (math/round (/ diff-secs year-seconds))]
        (if (< 1 n-years)
          (string n-years " years ago")
          (string n-years " year ago")))

      (< month-seconds diff-secs)
      (let [# months can be skewed for using 30 days per month, so ceil rather than round
            n-month (math/ceil (/ diff-secs month-seconds))]
        (if (< 1 n-month)
          (string n-month " months ago")
          (string n-month " month ago")))

      (< day-seconds diff-secs)
      (let [n-day (math/round (/ diff-secs day-seconds))]
        (if (< 1 n-day)
          (string n-day " days ago")
          (string n-day " day ago")))

      (< hour-seconds diff-secs)
      (let [n-hour (math/round (/ diff-secs hour-seconds))]
        (if (< 1 n-hour)
          (string n-hour " hours ago")
          (string n-hour " hour ago")))

      (< minute-seconds diff-secs)
      (let [n-minute (math/round (/ diff-secs minute-seconds))]
        (if (< 1 n-minute)
          (string n-minute " minutes ago")
          (string n-minute " minute ago")))

      :else
      "moments ago")))

(comment
 "20150830T123600Z"
 (os/time)                      # in seconds!

 1635012296
 (time-ago-str (amz-time-to-seconds "20150830T123600Z")
               (amz-time-to-seconds "20150902T123600Z"))

 (time-ago-str (amz-time-to-seconds "20210930T123600Z")
               (os/time))

 (scan-number "02")
 (os/date (amz-time-to-seconds "20150902T123600Z"))
 )
