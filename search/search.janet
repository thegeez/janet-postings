(import ./../lib/sqlite3 :as sql)

(def db-box @[nil])

(defn get-db []
  (if-let [db (get db-box 0)]
    db # TODO should check if handle still works?
    (let [db (if (os/getenv "JANET_DEV_ENV")
               (sql/open "test.db")
               #(sql/open ".db")
               # (do (eprintf "TODO define path to db on EFS")
               #     (error "TODO define path to db on EFS"))
               (sql/open "/mnt/test/test.db")
               )]
      (put db-box 0 db)
      db)))

(def search-pq-box @[nil])

(defn get-search-pq [db]
  (if-let [search-pq (get search-pq-box 0)]
    search-pq
    (let [search-pq (sql/prepare db "SELECT  sf.pk, sf.sk, snippet(search_fts, 2, '__b__', '__/b__', '...', 64) as title_highlight, snippet(search_fts, 3, '__b__', '__/b__', '...', 64) as text_highlight, sf.type as type, sf.username as username, sf.userslug as userslug, sf.cdate as cdate, sf.comment_count as comment_count, rank, s1.title as post_title FROM search_fts sf LEFT JOIN search s1 ON sf.type = \"COMMENT\" AND s1.pk = sf.pk AND s1.sk = \"A##POST\" WHERE search_fts MATCH :query ORDER BY rank LIMIT :limit OFFSET :offset")]
      (put search-pq-box 0 search-pq)
      search-pq)))

(def search-count-pq-box @[nil])

(defn get-search-count-pq [db]
  (if-let [search-count-pq (get search-count-pq-box 0)]
    search-count-pq
    (let [search-count-pq (sql/prepare db "SELECT count(*) as count FROM search_fts sf WHERE search_fts MATCH :query")]
      (put search-count-pq-box 0 search-count-pq)
      search-count-pq)))

(defn search [event]
  (let [query (get event "query")
        limit (get event "limit" 10)
        offset (get event "offset" 0)

        db (get-db)
        search-pq (get-search-pq db)
        search-count-pq (get-search-count-pq db)]
    (try
      (let [items (sql/execute search-pq {:query query
                                          :limit limit
                                          :offset offset})
            total-count (sql/execute search-count-pq {:query query})]
        {"items" items
         "total-count" (get-in total-count [0 :count])})
      ([err]
       (put search-pq-box 0 nil) # reset prepared statement
       (put search-count-pq-box 0 nil) # reset prepared statement
       (eprintf "Error search with event: %p" event)
       {:error "Search error"}))))

(comment
 (def db (get-db))
 (def pq (get-search-pq db))
 (sql/execute pq {:query "first OR" :limit 10 :offset 0})
 (put search-pq-box 0 nil)
 )




(def upsert-pq-box @[nil])

(defn get-upsert-pq [db]
  (if-let [upsert-pq (get upsert-pq-box 0)]
    upsert-pq
    (let [# warning INSERT OR REPLACE corrupts fts5 index!, must use INSERT .. ON CONFLICT
          upsert-pq (sql/prepare db "INSERT INTO search(pk, sk, title, text, username, userslug, cdate, comment_count, type) VALUES(:pk, :sk, :title, :text, :username, :userslug, :cdate, :comment_count, :type) ON CONFLICT DO UPDATE set title = :upd_title, text = :upd_text, username = :upd_username, userslug = :upd_userslug, cdate = :upd_cdate, comment_count = :upd_comment_count, type = :upd_type WHERE pk = :upd_pk AND sk = :upd_sk")
          # upsert-pq (sql/prepare db "INSERT OR REPLACE INTO search(pk, sk, title, text, username, userslug, cdate, comment_count, type) VALUES(:pk, :sk, :title, :text, :username, :userslug, :cdate, :comment_count, :type)")
          ]
      (put upsert-pq-box 0 upsert-pq)
      upsert-pq)))

(defn table->upsert-table [record]
  (let [{:title title
         :text text
         :username username
         :userslug userslug
         :cdate cdate
         :pk pk
         :sk sk
         :type type
         :comment_count comment_count} record]
    (merge record
           {:upd_title title
            :upd_text text
            :upd_username username
            :upd_userslug userslug
            :upd_cdate cdate
            :upd_pk pk
            :upd_sk sk
            :upd_type type
            :upd_comment_count comment_count})))

(defn import-records [db records]
  (let [upsert-pq (get-upsert-pq db)]
    (sql/execute-many upsert-pq (map table->upsert-table records))))

(defn handle-import [event]
  (def db (get-db))
  (let [records (map
                 (fn [record]
                   (let [image (get-in record ["dynamodb" "NewImage"])
                         sk (get-in image ["sk" "S"])
                         type (if (= sk "A##POST")
                                "POST"
                                "COMMENT")]
                     {:title (if (= type "POST") # comment don't have title to search in
                               (get-in image ["title" "S"])
                               :sqlnull)
                      :text (get-in image ["text" "S"])
                      :username (get-in image ["username" "S"])
                      :userslug (get-in image ["userslug" "S"])
                      :cdate (get-in image ["cdate" "S"])
                      :pk (get-in image ["pk" "S"])
                      :sk (get-in image ["sk" "S"])
                      :type type
                      :comment_count (if (= type "POST")
                                       (get-in image ["comment-count" "N"])
                                       :sqlnull)}))
                 (get event "Records"))]
    (import-records db records)))

(defn report []
  (def db (get-db))
  {"Count-q" (sql/eval db "SELECT count(*) as count FROM search")
   "First10" (sql/eval db "SELECT * FROM search LIMIT 10")})

(comment
 (def db (sql/open "test.db"))
 
 #(sql/eval db `CREATE TABLE customers(id INTEGER PRIMARY KEY, name TEXT);`)

 (sql/eval db `INSERT INTO customers VALUES(:id, :name);` {:name "John" :id 12345})

 (sql/eval db `SELECT * FROM search;`)

 #(sql/eval db `CREATE VIRTUAL TABLE search USING fts5(title, body);`)

 (sql/eval db `INSERT INTO search VALUES(:title, :body);` {:title "The first post title"
                                                           :body "The body is here folks!"})
 (sql/eval db `INSERT INTO search VALUES(:title, :body);` {:title "There is a second post"
                                                           :body "Janet works with search?!"})

 (def p1 (sql/prepare db `INSERT INTO search VALUES(:title, :body);`))
 (sql/execute-many p1 [{:title "There is a second post"
                        :body "Janet works with search?!"}
                       {:title "one"
                        :body "LOL"}])

 # highlight(email, 2, '<b>', '</b>')
 # SELECT * FROM email WHERE email MATCH 'fts5' ORDER BY rank;

 (sql/eval db `SELECT highlight(search, 0, '<b>', '</b>') as title, body as body, rank as rank FROM search WHERE search MATCH ? ORDER BY rank` ["post NOT second"])


 (def p2 (sql/prepare db `SELECT highlight(search, 0, '<b>', '</b>') as title, body as body, rank as rank FROM search WHERE search MATCH ? ORDER BY rank`))

 (sql/execute p2 ["post NOT second"])


 # check if UNIQUE key on post id can replace content to remove old entries

 (sql/eval db `DROP TABLE search2`)
 (sql/eval db `CREATE VIRTUAL TABLE search2 USING fts5(id UNINDEXED, title, body);`)

 (def p1 (sql/prepare db `INSERT INTO search2(title, body) VALUES(:title, :body);`))
 (sql/execute-many p1 [{:title "ONE"
                        :body "Body one"}
                       {:title "two"
                        :body "Body two"}])

 (sql/eval db `SELECT rowid,* from search2`)

 (def p3 (sql/prepare db `SELECT rowid,* from search2 WHERE id = ?`))
 (sql/execute-many p3 [["AJCKSFAHSF923_LOL1234"] ["AJCKSFAHSF923_LOL444"]])

 (sql/eval db "DELETE FROM search2 WHERE id = ?" ["AJCKSFAHSF923_LOL444"])
 (sql/eval db "INSERT INTO search2(id, title, body) VALUES('AJCKSFAHSF923_LOL4444423', 'title444', 'body444') RETURNING last_insert_rowid() as rowid")
 

 (sql/eval db `UPDATE search2 SET title = ?, body = ? WHERE id = ?` ["new title" "new body" "AJCKSFAHSF923_LOL1234"])

 (sql/eval db "REPLACE INTO search2(id, title, body) VALUES('AJCKSFAHSF923_LOL', 'title', 'body')")
 (sql/eval db ` INTO  search2 SET title = ?, body = ? WHERE id = ?` ["new title" "new body" "AJCKSFAHSF923_LOL1234"])
 (sql/last-insert-rowid db)

 (sql/eval db `EXPLAIN SELECT rowid,* from search2 WHERE id = ?`)

 (sql/eval db "SELECT * FROM search")

 )

(defn bootstrap-tables [db]
# from https://www.sqlite.org/fts5.html
  (sql/eval db `  -- Create a table. And an external content fts5 table to index it.
CREATE TABLE search(pk TEXT, sk TEXT, title, text, username, userslug, cdate, comment_count, type, UNIQUE (pk, sk));
CREATE VIRTUAL TABLE search_fts USING fts5(pk UNINDEXED, sk UNINDEXED, title, text, username UNINDEXED, userslug UNINDEXED, cdate UNINDEXED, comment_count UNINDEXED, type UNINDEXED, content='search', content_rowid='rowid');

-- Triggers to keep the FTS index up to date.
CREATE TRIGGER search_after_ins AFTER INSERT ON search BEGIN
  INSERT INTO search_fts(rowid, pk, sk, title, text, username, userslug, cdate, comment_count, type) VALUES (new.rowid, new.pk, new.sk, new.title, new.text, new.username, new.userslug, new.cdate, new.comment_count, new.type);
END;
CREATE TRIGGER search_after_del AFTER DELETE ON search BEGIN
  INSERT INTO search_fts(search_fts, rowid, pk, sk, title, text, username, userslug, cdate, comment_count, type) VALUES('delete', old.rowid, old.pk, old.sk, old.title, old.text, old.username, old.userslug, old.cdate, old.comment_count, old.type);
END;
CREATE TRIGGER search_after_upd AFTER UPDATE ON search BEGIN
  INSERT INTO search_fts(search_fts, rowid, pk, sk, title, text, username, userslug, cdate, comment_count, type) VALUES('delete', old.rowid, old.pk, old.sk, old.title, old.text, old.username, old.userslug, old.cdate, old.comment_count, old.type);
  INSERT INTO search_fts(rowid, pk, sk, title, text, username, userslug, cdate, comment_count, type) VALUES (new.rowid, new.pk, new.sk, new.title, new.text, new.username, new.userslug, new.cdate, new.comment_count, new.type);
END;
`))

(defn drop-bootstrap-tables [db]
  (sql/eval db `
DROP TRIGGER search_after_ins;
DROP TRIGGER search_after_del;
DROP TRIGGER search_after_upd;
DROP TABLE search_fts;
DROP TABLE search;
`))

(defn bootstrap []
  (def db (get-db))
  (bootstrap-tables db)
  (report)
  )

(defn destroy []
  (def db (get-db))
  (drop-bootstrap-tables db))


(comment
 (sql/eval db "DROP TABLE search; DROP TABLE search_fts")
 )

(comment

 (import-bulk [{:id "AAAAA"
                :title "First post"
                :body "First body"}
               {:id "BBBBB"
                :title "Second post"
                :body "Second body"}
               {:id "CCCCC"
                :title "Third post"
                :body "Third body"}])

 (sql/eval db "SELECT * FROM search")
 (sql/eval db "SELECT *,rowid FROM search_fts")

 (sql/eval db "SELECT id, highlight(search_fts, 1, '<b>', '</b>') as title, highlight(search_fts, 2, '<b>', '</b>') as body FROM search_fts WHERE search_fts MATCH \"updated\" ")

 (sql/eval db "SELECT id, highlight(search_fts, 1, '<b>', '</b>') as title, highlight(search_fts, 2, '<b>', '</b>') as body FROM search_fts WHERE search_fts MATCH \"First\" ")

 (sql/eval db "SELECT id, highlight(search_fts, 1, '<b>', '</b>') as title, highlight(search_fts, 2, '<b>', '</b>') as body FROM search_fts WHERE search_fts MATCH \"Second\" ")

 (sql/eval db "DELETE FROM search WHERE id = \"BBBBB\" ")
 
 (import-bulk [{:id "AAAAA"
                :title "First post"
                :body "First body UPDATED"}
               {:id "CCCCC"
                :title "Third post UPDATED"
                :body "Third body"}])
 )

(comment
 (os/cwd)
 (os/cd "..")
 (def db (get-db))
 (drop-bootstrap-tables db)
 (bootstrap-tables db)

 (sql/close db)
 (sql/eval db "select * from search")

 (sql/eval db "select count(*) from search")

 (sql/eval db "select * from search_fts WHERE search_fts MATCH 'comment'")

 (sql/eval db "SELECT pk, sk, snippet(search_fts, 2, '__b__', '__/b__', '...', 64) as title_highlight, snippet(search_fts, 3, '__b__', '__/b__', '...', 64) as text_highlight, * FROM search_fts WHERE search_fts MATCH \"Second OR THIRD OR fourth\" ")

 # add titles on comments
 (sql/eval db "SELECT  sf.pk, sf.sk, snippet(search_fts, 2, '<b>', '</b>', '...', 64) as title_highlight, snippet(search_fts, 3, '<b>', '</b>', '...', 64) as text_highlight, sf.type as type, rank, s1.title as post_title FROM search_fts sf LEFT JOIN search s1 ON sf.type = \"COMMENT\" AND s1.pk = sf.pk AND s1.sk = \"A##POST\" WHERE search_fts MATCH ? ORDER BY rank LIMIT 1 OFFSET 1" ["Second OR THIRD OR fourth OR first"])
 (put upsert-pq-box 0 nil)
 (get-upsert-pq db)


 (sql/eval db
           "INSERT OR REPLACE INTO search(pk, sk, title, text, username, userslug, cdate, comment_count, type) VALUES(:pk, :sk, :title, :text, :username, :userslug, :cdate, :comment_count, :type);"
           {:title "title goes here" #:sqlnull
            :text "Text"
            :username "username"
            :userslug "userslug"
            :cdate "cdate"
            :pk "pk"
            :sk "sk"
            :type "POST"
            :comment_count 12

              #:sqlnull
           })

(sql/eval db
          ` -- Create a table. And an external content fts5 table to index it.
CREATE TABLE tbl(b TEXT UNIQUE, c, d);
CREATE VIRTUAL TABLE fts_idx USING fts5(b, c, d, content='tbl', content_rowid='rowid');

-- Triggers to keep the FTS index up to date.
CREATE TRIGGER tbl_ai AFTER INSERT ON tbl BEGIN
  INSERT INTO fts_idx(rowid, b, c, d) VALUES (new.rowid, new.b, new.c, new.d);
END;
CREATE TRIGGER tbl_ad AFTER DELETE ON tbl BEGIN
  INSERT INTO fts_idx(fts_idx, rowid, b, c, d) VALUES('delete', old.rowid, old.b, old.c, old.d);
END;
CREATE TRIGGER tbl_au AFTER UPDATE ON tbl BEGIN
  INSERT INTO fts_idx(fts_idx, rowid, b, c, d) VALUES('delete', old.rowid, old.b, old.c, old.d);
  INSERT INTO fts_idx(rowid, b, c, d) VALUES (new.rowid, new.b, new.c, new.d);
END;`)

(sql/eval db "SELECT *,rowid FROM tbl")
(sql/eval db "INSERT INTO tbl(b,c, d) VALUES(?,?,?)" ["LOL" "YES" "ONE"])
(sql/eval db "INSERT INTO tbl(b,c, d) VALUES(?,?,?)" ["AAA" "BBB" "CCC"])
(sql/eval db "INSERT INTO tbl(b,c, d) VALUES(?,?,?)" ["LOL" "YES" "TWO"])
(sql/eval db "INSERT INTO tbl(b,c,d) VALUES(?,?,?) ON CONFLICT DO UPDATE set c = ?, d = ? WHERE b = ?" ["LOL" "YES" "TWO"
                                                                                                        "YES" "TWO"
                                                                                                        "LOL"])

(sql/eval db "INSERT OR REPLACE INTO tbl(b,c,d) VALUES(?,?,?)" ["LOL" "YES" "THREE"])

(sql/eval db "SELECT * FROM fts_idx WHERE fts_idx MATCH ?"
          ["\"AAA\" OR \"LOL\""]
          #["LOL"]
          )
 )
