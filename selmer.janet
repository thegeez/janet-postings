(def- escape-peg
  (peg/compile
   ~(% (any (+ (* "&" (constant "&amp;"))
               (* "\"" (constant "&quot;"))
               (* "<" (constant "&lt;"))
               (* ">" (constant "&gt;"))
               (* "'" (constant "&#39;"))
               '1)))))

(defn escape-html [x]
  (in (peg/match escape-peg (string x)) 0))

(def grammar
  (peg/compile
   ~{:main (sequence :root
                     -1)
     :root (any (choice
                 :tagged
                 :keep-content))
     :keep-content
     (replace (capture (any (if-not (choice :tag-open :tag-close)
                              1)))
              ,(fn [c]
                 [:keep c]))

     :tag-open (choice :tag-open-inline
                       :tag-open-flow
                       :tag-open-code
                       :tag-open-comment)
     :tag-open-inline "{{"
     :tag-open-flow "{%"
     :tag-open-code "{-"
     :tag-open-comment "{#"
     :tag-close (choice :tag-close-inline
                        :tag-close-flow
                        :tag-close-code
                        :tag-close-comment)
     :tag-close-inline "}}"
     :tag-close-flow "%}"
     :tag-close-code "-}"
     :tag-close-comment "#}"
     :tagged (choice :tagged-inline
                     :tagged-flow
                     :tagged-comment)
     :tagged-inline (replace (sequence :tag-open-inline
                                       (replace (capture :root)
                                                ,(fn [[_keep c] match]
                                                   c))
                                       :tag-close-inline)
                             ,(fn [c]
                                [:inline c ]))
     :tagged-comment (sequence :tag-open-comment
                               (drop :root)
                               :tag-close-comment)
     :tagged-flow (choice :tagged-if
                          :tagged-for
                          :tagged-include
                          :tagged-code
                          :tagged-do
                          :tagged-block
                          :tagged-extends
                          )
     :tagged-if (replace (capture (sequence :tag-open-flow " if "
                                            (replace (capture (to (sequence " " :tag-close-flow)))
                                                     ,(fn [code]
                                                        [:code code]))
                                            " " :tag-close-flow
                                            (replace (capture :root)
                                                     ,(fn [& c]
                                                        [:then (splice (array/slice c 0 -2))]))
                                            (choice (sequence :tag-open-flow " endif " :tag-close-flow)
                                                    (sequence :tag-open-flow " else " :tag-close-flow
                                                              (replace (capture :root)
                                                                       ,(fn [& c]
                                                                          [:else (splice (array/slice c 0 -2))]))
                                                              :tag-open-flow" endif " :tag-close-flow))))
                         ,(fn [check then else &opt match]
                            (if match
                              [:if check then else]
                              [:if check then])))
     :tagged-for (replace (capture (sequence :tag-open-flow " for "
                                             (replace (capture (to " in "))
                                                      ,(fn [code]
                                                         [:for-bind code]))
                                             " in "
                                             (replace (capture (to (sequence " " :tag-close-flow)))
                                                      ,(fn [code]
                                                         [:for-from code]))
                                             " " :tag-close-flow
                                             (replace (capture :root)
                                                      ,(fn [& c]
                                                         [:body (splice (array/slice c 0 -2))]))
                                             (sequence :tag-open-flow " endfor " :tag-close-flow)))
                          ,(fn [for-bind for-from body match]
                             [:for for-bind for-from body]))
     :tagged-include (sequence :tag-open-flow " include \""
                               (replace (capture (to (sequence "\" " :tag-close-flow)))
                                        ,(fn [code]
                                           [:include code]))
                               "\" " :tag-close-flow
                               )
     :tagged-code (sequence :tag-open-code
                            (replace (capture (to (sequence " " :tag-close-code)))
                                     ,(fn [code]
                                        [:code code]))
                            " " :tag-close-code
                            )
     :tagged-do (sequence :tag-open-flow " do " :tag-close-flow
                          (replace (capture :root)
                                   ,(fn [& c]
                                      [:body (splice (array/slice c 0 -2))]))
                          (sequence :tag-open-flow " enddo " :tag-close-flow))

     :tagged-block (replace (sequence :tag-open-flow " block "
                                      (replace (capture (to (sequence " " :tag-close-flow)))
                                               ,(fn [name]
                                                  [:block-name name]))
                                      " " :tag-close-flow
                                      (replace (capture :root)
                                               ,(fn [& c]
                                                  [:body (splice (array/slice c 0 -2))]))
                                      (sequence :tag-open-flow " endblock " :tag-close-flow (choice "\n" "")))
                            ,(fn [block-name body]
                               [:block block-name body]))
     :tagged-extends (sequence :tag-open-flow " extends \""
                               (replace (capture (to (sequence "\" " :tag-close-flow)))
                                        ,(fn [filename]
                                           [:extends filename]))
                               "\" " :tag-close-flow (choice "\n" "")
                               )
     }))

(defn str->ast [in]
  (peg/match grammar
             in))

(defn ast->code [form &opt blocks]
  (if (= (type form) :tuple)
    (let [op (get form 0)]
      (case op
        :keep (tuple 'do (splice (map (fn [line]
                                        (tuple 'buffer/push-string '__0b line)) (drop 1 form))))
        :code (parse (string "(upscope " (get form 1) ")"))
        :if (let [code (get form 1)
                  then (get form 2)]
              (if-let [else (get form 3)]
                (tuple 'if code then else)
                (tuple 'if code then)))
        :then (tuple 'do (splice (drop 1 form)))
        :else (tuple 'do (splice (drop 1 form)))
        :for (let [for-bind (get form 1)
                   for-from (get form 2)
                   body (get form 3)]
               (tuple 'do
                       (tuple 'var 'for_index 0)
                       (tuple 'loop [for-bind :in for-from]
                               body
                               (tuple '++ 'for_index))))
        :for-bind (symbol (get form 1))
        :for-from (parse (get form 1))
        :body (tuple 'do (splice (drop 1 form)))
        :inline (tuple 'buffer/push-string '__0b (tuple 'escape-html (parse (get form 1))))
        :include2 (tuple 'do (splice (map (fn [line]
                                           (tuple 'buffer/push-string '__0b "%include%" line)) (drop 1 form))))
        # todo cache load+parse
        :include (postwalk ast->code (str->ast (slurp (get form 1))))
        :extends (tuple :extends
                        (get form 1))
        :block (let [block-name (get form 1)]
                 (if blocks # doing ast->code for filling in an extends template
                   # include block in extends
                   (if-let [insert-block (get blocks block-name)]
                     (tuple 'do (splice insert-block))
                     nil # block in extendable template is not overwritten
                     )
                   # defining blocks to use in extends
                   (let [block-form (get form 2)]
                     (tuple :block
                            block-name
                            block-form))))
        :block-name (get form 1)

        (errorf "No ast clause for %p" form)))
    form))

(defn make-render [in &opt args]
  (def supplied (or args @{}))
  (let [ast (str->ast in)
        code (postwalk
              ast->code
              ast)

        code (if (= (-> code first first) :extends)
               (let [filename (get-in code [0 1])
                     in (slurp filename)
                     blocks (reduce
                              (fn [m block]
                                (put m (get block 1) (drop 2 block)))
                              @{}
                              (drop 1 code))
                     ast (str->ast in)
                     code (postwalk (fn [form]
                                      (ast->code form blocks)) ast)]
                 code)
               code)

        _ (assert (not= (-> code first first) :extends)
                  "Can't have nested :extends")

        code (tuple 'fn 'template-fn '[args]
                     (tuple 'let ['__0b (tuple 'buffer/new 1024)]
                             (splice code)))
        comp-env (table/setproto @{'getx @{:value get}
                                   'escape-html @{:value escape-html}} (make-env))
        fn-asm (compile code comp-env)
        _ (when (get fn-asm :error)
            (printf "Error on compile %p" fn-asm)
            (printf "code %p" code))
        temple-fib (fiber/new fn-asm :e)
        _ (fiber/setenv temple-fib comp-env)
        template-fn (resume temple-fib)]
    template-fn
    ))

(defn render [in &opt args]
  (def supplied (or args @{}))
  (let [template-fn (make-render in)]
    (template-fn supplied)))


(defn make-template-fn [file-location]
  (if (os/getenv "JANET_DEV_ENV")
    (fn [args]
      (try (let [template-fn (-> file-location
                                 slurp
                                 make-render)
                ]
             (template-fn args))
           ([err]
            (string/format "TEMPLATE_ERROR %p %v" err err)
            )))
    ## normal case
    (let [template-fn (-> file-location
                          slurp
                          make-render)]
      # (fn [args] ... buffer out ...)
      template-fn)))


(comment render "{% if (= (getx args :username) \"bob\") %} hi {% else %} lol {% endif %}")

(comment render "before {% if true %} hi {% if true %}!{% else %}?{% endif %}{% else %} lol {% endif %}after")

(comment render "12{{345}}678")
(comment render "12{% if true %}3{{345}}45{% endif %}67")
(comment render "Hello {{(getx args :name)}}" {:name "Your name"})

(comment render "{{(getx args :name)}} lol {# hi #} Hahaha" {:name "Your name"})

(comment render "<ul>
{% for item in (getx args :items) %}
    <li>{{item}}</li>
{% endfor %}
</ul>" {:items [1 2 3]})

(comment render "<ul>
{% for item in (getx args :items) %}
{- (def item (get item :a)) -}
    <li>{{item}}{% include \"sometemplate\" %} index {{ for_index }}</li>
{% endfor %}
</ul>" {:items [{:a 1} {:a "<h1>2</h1>"} {:a 3}]})


(comment render "Hello this is a scope
{% do %}
{- (def item \"some <b>string</b>\") -}
    <h1>{{item}}</h1>
{% enddo %}
item not def'd here" {})

(comment render "Hello world")


(comment render (slurp "index.html") {:items [1 2 3]})

(comment render "{- (var i 1) (++ i) -}i is: {{i}}")

