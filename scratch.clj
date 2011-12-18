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

(def all-posts
  (let [conn (SqlConnection. "Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CommunityServer;Data Source=.")
        _ (.Open conn)]
    (select conn "select PostID, ParentID, Body, IsApproved, PostAuthor, PostType, PostName, PostDate, Subject, PostLevel from dbo.cs_Posts")))

(comment (def threads (let [conn (SqlConnection. "Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CommunityServer;Data Source=.")
                    _ (.Open conn)]
                (select conn "select * from dbo.cs_Threads"))))


(defn approved?
  "Return true if the post has been approved."
  [post]
  (get post "IsApproved"))

(defn post-id
  "Return the id of this post."
  [post]
  (get post "PostID"))

(defn post-parent
  "Return the ID of the parent of this post."
  [post]
  (get post "ParentID"))

(defn post-author
  "Return the author of this post"
  [post]
  (get post "PostAuthor"))

(defn post-title
  "Return the title of this post"
  [post]
  (get post "Subject"))

(defn toplevel?
  "Return true if this post is top level (i.e. it is its own parent)."
  [post]
  (= (post-id post) (post-parent post)))

(defn comment?
  "Return true if this post is a comment."
  [post]
  (not (toplevel? post)))

(def posts (filter approved? all-posts))

(def toplevel-posts (filter toplevel? posts))

(defn comments
  "Given the list of all posts, return a seq of the posts that are
  comments on that particular post."
  [all-posts post]
  (let [id (post-id post)]
    (filter #(and (not (toplevel? %))
                  (= id (post-parent %)))
            all-posts)))

(def posts-with-comments
  (map #(assoc % :comments (comments posts %)) toplevel-posts))

(def content-by-author
  (group-by post-author posts-with-comments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(System.Reflection.Assembly/LoadWithPartialName "System.Xml")
(import '[System.Xml XmlTextWriter Formatting XmlConvert])
(import 'System.DateTime)

(def atom-ns "http://www.w3.org/2005/Atom")

;; Comments have postlevel = 2 and ParentID != PostID

(defn write-entry
  "Write a Atom <entry>s to the XmlWriter."
  [writer id time title body author comments]
  (doto writer
    (.WriteStartElement "entry" atom-ns)
    (.WriteElementString "id" atom-ns id)
    (.WriteElementString "published" atom-ns (XmlConvert/ToString time))
    (.WriteElementString "updated" atom-ns (XmlConvert/ToString time))

    (.WriteStartElement "category" atom-ns)
    (.WriteAttributeString "scheme" "http://schemas.google.com/g/2005#kind")
    (.WriteAttributeString "term"
                           (if (toplevel? post)
                             "http://schemas.google.com/blogger/2008/kind#post"
                             "http://schemas.google.com/blogger/2008/kind#comment"))
    (.WriteEndElement)

    (.WriteStartElement "title" atom-ns)
    (.WriteAttributeString "type" "text")
    (.WriteValue (post-title post))
    (.WriteElement)

    (.WriteStartElement "content" atom-ns)
    (.WriteAttributeString "type" "html")
    (.WriteValue (post-body post))
    (.WriteEndElement)

    (.WriteStartElement "author" atom-ns)
    (.WriteElementString "name" atom-ns author)
    (.WriteEndElement))
  (if comments
    (throw (Exception. "Here's where I was.")))
  )

(defn write-post-entry
  "Given a post (with comments) write the entries for it to the XmlWriter."
  [writer post]
  (write-entry writer
               (post-id post)
               (post-time post)
               (post-title post)
               (post-body post)
               (post-author post)
               (:comments post)))

(defn write-post-entries
  "Given a seq of posts (with comments), write the entries for them to
  the XmlWriter."
  [writer posts]
  (doseq [post posts] (write-post-entry writer post)))

(defn write-feed
  "Spit out an Atom feed file with the specified name, given the threads."
  [path posts]
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



