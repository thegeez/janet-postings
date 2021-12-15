(declare-project
 :name "postings"
 :dependencies [#"https://github.com/joy-framework/http"
                "https://github.com/joy-framework/halo2"
                "https://github.com/janet-lang/json"])

(declare-executable
 :name "bootstrap"
 :entry "service.janet"
 )

