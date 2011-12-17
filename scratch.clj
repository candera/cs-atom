(System.Reflection.Assembly/LoadWithPartialName "System.Data")
(import '[System.Data.SqlClient SqlConnection SqlCommand])
;; (def conn (SqlConnection. "Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CommunityServer;Data Source=."))

;; (.Open conn)

;; (def cmd (.CreateCommand conn))

;; (.set_CommandText cmd "select * from dbo.cs_Posts")

;; (def reader  (.ExecuteReader cmd))

;; (.Read reader)

;; (.get_FieldCount reader)

;; (.GetValue reader 2)
;; (.GetName reader 2)

;; (.Close reader)

;; (doc range)

(defn row
  "Given a SqlReader, produce a sequence of all the values in the
  current row."
  [reader]
  (doall (map #(.GetValue reader %) (range 0 (.get_FieldCount reader)))))

(defn rows
  "Given a SqlReader, produce a sequence of sequences of the values in
  one row."
  [reader]
  (loop [result []]
    (if (.Read reader)
      (recur (conj result (row reader)))
      result)))

(defn column-names
  "Return a sequence the names of the columns in the specified
  SqlReader."
  [reader]
  (doall (map #(.GetName reader %) (range 0 (.get_FieldCount reader)))))

(defn select
  "Given a connection and some SQL, return all the result as a
  sequence of maps of column names to values."
  [conn sql]
  (let [cmd (SqlCommand. sql conn)
        reader (.ExecuteReader cmd)
        columns (column-names reader)
        result (map #(zipmap columns %) (rows reader))]
    (.Close reader)
    result))

(use 'clojure.pprint)

(def all-posts (let [conn (SqlConnection. "Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CommunityServer;Data Source=.")
                 _ (.Open conn)]
             (select conn "select * from dbo.cs_Posts")))

(def threads (let [conn (SqlConnection. "Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CommunityServer;Data Source=.")
                 _ (.Open conn)]
             (select conn "select * from dbo.cs_Threads")))


(def posts (group-by #(get % "ParentID") all-posts))

(def toplevel-posts (filter #(= (get % "PostID") (get % "ParentID")) all-posts))

(defn comments
  "Given the list of all posts, return a seq of the posts that are
  comments on a particular post."
  [all-posts post]
  )

(def posts-with-comments
  (map #() (filter (fn [p] ((get p "ParentID"))) toplevel-posts)))

(comment (binding [*print-level* 3] (pprint posts)))

(def authors (distinct (map #(get % "PostAuthor") threads)))

(def threads-by-author (group-by #(get % "PostAuthor") threads))

(defn posts-for-thread
  "Filter a seq of posts for a given thread."
  [posts thread]
  (filter #(= (get thread "ThreadID") (get % "ThreadID")) posts))

(comment (->> (threads-by-author "craig-andera")
              (take 3)
              (posts-for-thread posts)
              (take 5)
              pprint
              ))

(def threads-with-posts
  (map #(assoc % :posts (posts-for-thread posts %)) threads))

;; (binding [*print-length* 10] (pprint (take 1 threads-with-posts)))

;; ((first threads-with-posts) "PostAuthor")

;; (pprint (threads-by-author "craig-andera"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(System.Reflection.Assembly/LoadWithPartialName "System.Xml")
(import '[System.Xml XmlTextWriter Formatting XmlConvert])
(import 'System.DateTime)

;; (def writer (XmlTextWriter. "C:\\temp\\test.atom" nil))

;; (.set_Formatting writer Formatting/Indented)

;; (.WriteElementString writer "test" "foo")

;; (.Close writer)

(def atom-ns "http://www.w3.org/2005/Atom")

;; Comments have postlevel = 2 and ParentID != PostID

(defn write-post-entry
  "Given a thread (with posts) write the entries for it to the XmlWriter."
  [writer thread]
  (write-entry writer
               (thread "ThreadID")      ; id
               (thread "PostDate")      ; published & updated
               (thread "ThreadDate")    ; title
               ))

(defn write-post-entries
  "Given a seq of threads (with posts), write the entries for them to
  the XmlWriter."
  [writer threads]
  (doall (map (partial write-post-entry writer) threads)))

(defn write-feed
  "Spit out an Atom feed file with the specified name, given the threads."
  [path threads]
  (doto (XmlTextWriter. path nil)
    (.set_Formatting Formatting/Indented)
    (.WriteStartElement "feed" atom-ns)
    (.WriteElementString "id" atom-ns "feed-id")
    (.WriteElementString "updated" atom-ns (XmlConvert/ToString (DateTime/Now)))

    (.WriteStartElement "title" atom-ns)
    (.WriteAttributeString "type" "text")
    (.WriteValue "Blog Title Here")
    (.WriteEndElement)

    (.WriteStartElement "generator" atom-ns)
    (.WriteAttributeString "version" "7.00")
    (.WriteAttributeString "uri" "http://www.blogger.com")
    (.WriteValue "Blogger")
    (.WriteEndElement)

    (write-post-entries threads)

    (.WriteStartElement "entry" atom-ns)
    (.WriteValue "TODO")
    (.WriteEndElement)

    (.Close)))

(comment (write-feed "C:\\temp\\test3.atom" threads))

(def some-threaded-posts
  (let [some-threads (->> (threads-by-author "craig-andera")
                          (take 3))]
    (map #(assoc % :posts (take 3 (posts-for-thread posts %))) some-threads)))

(pprint some-threaded-posts)