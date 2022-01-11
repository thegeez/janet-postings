(declare-project
 :name "postings"
 :dependencies ["https://github.com/joy-framework/halo2"
                "https://github.com/janet-lang/json"
                {:repo "https://github.com/thegeez/jalmer" :tag "main"}])

(declare-executable
 :name "bootstrap"
 :entry "service.janet"
 )

(declare-executable
 :name "bootstrap_search"
 :entry "search/service.janet"
 )
