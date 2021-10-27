# from https://git.sr.ht/~bakpakin/temple/blob/master/temple.janet (can't clone git repo from sr.ht on this machine?!)
###
### temple.janet
### Copyright © Calvin Rose 2020
###

(defmacro- defenv
  "Define a module inline as if returned by require."
  [what & forms]
  (def env (make-env))
  (each f forms
    (resume (fiber/setenv (coro (eval f)) env)))
  ~(def ,what ',env))

(defenv base-env
  # Define forms available inside the temple DSL here
  (def- escape-peg
    (peg/compile
     ~(% (any (+ (* "&" (constant "&amp;"))
                 (* "\"" (constant "&quot;"))
                 (* "<" (constant "&lt;"))
                 (* ">" (constant "&gt;"))
                 (* "'" (constant "&#39;"))
                 '1)))))
  (defn escape [x]
    (in (peg/match escape-peg (string x)) 0))

  (defmacro never-nil [x]
    ~(or ,x
          (errorf "never-nil failed: %p" ',x)))

  (defmacro getx [& more]
    ~(or (get ,(splice more))
         (errorf "getx failed: %p" '(getx ,(splice more)))))

  (defmacro get-inx [& more]
    ~(or (get-in ,(splice more))
         (errorf "getx failed: %p" '(get-inx ,(splice more))))))

(defn source-includes [source]
  (try
    (let [out @""
          parts (string/split "{@include " source)]
      # string/find patt str &opt start-index
      (buffer/push-string out (get parts 0))
      (loop [i :range [1 (length parts)]]
        (let [part (get parts i)
              [include-filename-part template] (string/split "@}" part)
              include-filename (string/trim include-filename-part)
              include-source (slurp include-filename)
              include-source-expanded (source-includes include-source)]
          (buffer/blit out include-source-expanded
                       (length out) # dest start
                       )
          (buffer/push-string out template)
          ))
      out
      )
    ([err]
     (string/format "Template include error: %p" err))
    ))

(defn source-extends-includes [source]
  (if (string/has-prefix? "{@" source)
    (try (let [[extends-tag template-source] (string/split "@}" source
                                                           0 # start
                                                           2 # limit, only split on closing @} for {@extends
                                                           )
               extends-filename (-> (string/split "{@extends " extends-tag)
                                    (get 1)
                                    (string/trim))
               extends-source (slurp extends-filename)
               merged-source (string/replace "{@block @}" template-source extends-source)]
           (source-includes merged-source)
           )
         ([err]
          (string/format "Template extends error: %p" err)))
    # source does not start with extends tag
    (source-includes source)))

(comment
 (def in (slurp "templates/foo.html"))
 (source-includes in)

 (def res (source-extends-includes in))

 )

(defn create
  "Compile a template string into a function. Optionally
  provide a location where the source is from to improve debugging. Returns
  the template function."
  [source &opt where]

  (default where source)

  (def source (source-extends-includes source))

  (def env (table/setproto @{} base-env))

  # Inherit dyns
  (let [current-env (fiber/getenv (fiber/current))]
    (loop [[k v] :pairs current-env :when (keyword? k)]
      (put env k v)))

  # State for compilation machine
  (def p (parser/new))
  (def forms @[])

  (defn compile-time-chunk
    "Eval the capture straight away during compilation. Use for imports, etc."
    [chunk]
    (defn do-in-env [] (eval-string chunk))
    (def f (fiber/new do-in-env))
    (fiber/setenv f env)
    (resume f)
    true)

  (defn parse-chunk
    "Parse a string and push produced values to forms."
    [chunk]
    (parser/consume p chunk)
    (while (parser/has-more p)
      (array/push forms (parser/produce p))))

  (defn code-chunk
    "Parse all the forms in str and insert them into the template."
    [str]
    (parse-chunk str)
    (if (= :error (parser/status p))
      (error (parser/error p)))
    true)

  (defn sub-chunk
    "Same as code-chunk, but results in sending code to the buffer."
    [str]
    (code-chunk
      (string "\n(prin (escape (do " str "\n))) ")))

  (defn raw-chunk
    "Same as code-chunk, but results in sending code to the buffer."
    [str]
    (code-chunk
      (string "\n(prin (do " str "\n)) ")))

  (defn string-chunk
    "Insert string chunk into parser"
    [str]
    (parse-chunk "\n")
    (parser/insert p ~(,prin ,str))
    true)

  # Run peg
  (def grammar
    ~{:code-chunk (* "{%" (drop (cmt '(any (if-not "%}" 1)) ,code-chunk)) "%}")
      :compile-time-chunk (* "{$" (drop (cmt '(any (if-not "$}" 1)) ,compile-time-chunk)) "$}")
      :sub-chunk (* "{{" (drop (cmt '(any (if-not "}}" 1)) ,sub-chunk)) "}}")
      :raw-chunk (* "{-" (drop (cmt '(any (if-not "-}" 1)) ,raw-chunk)) "-}")
      :main-chunk (drop (cmt '(any (if-not (+ "{$" "{{" "{%" "{-") 1)) ,string-chunk))
      :main (any (+ :compile-time-chunk :raw-chunk :code-chunk :sub-chunk :main-chunk (error "")))})
  (def did-match (peg/match grammar source))

  # Check errors in template and parser
  (unless did-match (error "invalid template syntax"))
  (parse-chunk "\n")
  (parser/eof p)
  (case (parser/status p)
    :error (error (parser/error p)))

  # Make ast from forms
  (def ast ~(fn temple-template [args]
              ,;forms
              nil))


  (def ctor (compile ast env (string where)))
  (if-not (function? ctor)
    (error (string "could not compile template: " (string/format "%p" ctor))))

  (let [f (fiber/new ctor :e)]
    (fiber/setenv f env)
    (def res (resume f))
    (case res
      :error (error res)
      res)))

(defn make-template-fn [file-location]
  (if (os/getenv "JANET_DEV_ENV")
    (fn [args]
      (try (let [template-fn (-> file-location
                                 slurp
                                 create)
                 b @""]
             (with-dyns [:out b]
               (template-fn args))
             b)
           ([err]
            (string/format "TEMPLATE_ERROR %p" err))))
    ## normal case
    (let [template-fn (-> file-location
                          slurp
                          create)]
      (fn [args]
        (let [b @""]
          (with-dyns [:out b]
            (template-fn args))
          b)))))
