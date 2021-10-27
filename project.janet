(declare-project
 :name "postings"
 :dependencies ["https://github.com/joy-framework/http"
                "https://github.com/joy-framework/halo2"
                "https://github.com/janet-lang/json"])

# build with jpm build
#(declare-native :name "dev_share" :source @["dev_share.c"])

# (declare-executable
#  :name "myexec"
#  :entry "hello.janet")

(declare-executable
 :name "bootstrap"
 :entry "service.janet"
 )

