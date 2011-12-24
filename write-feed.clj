;; Read in the data that go written out by the .NET export
(import 'java.io.PushbackReader)
(use 'clojure.java.io)
#_(def all-posts
  (let [source (PushbackReader. (reader "raw-data.clj"))]
    (loop [posts []]
      (let [post (read source false :done)]
        (if (= post :done)
          posts
          (recur
           (if (post "IsApproved")
             (conj posts post)
             posts)))))))

;; Helper functions for working with posts
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

(defn post-time
  "Return the publication time of a post"
  [post]
  (get post "PostDate"))

(defn post-body
  "Return the body of a post"
  [post]
  (get post "Body"))

(defn toplevel?
  "Return true if this post is top level (i.e. it is its own parent)."
  [post]
  (= (post-id post) (post-parent post)))

(defn comment?
  "Return true if this post is a comment."
  [post]
  (not (toplevel? post)))

(use 'net.cgrand.enlive-html)

(def empty-p-tags (pred #(and (= (:tag %) :p) (nil? (:content %)))))

(defn remove-blank-p-tags
  "Process a seq of nodes into a seq of nodes that have no empty p
  tags in them."
  [nodes]
  (transform nodes [empty-p-tags] (constantly nil)))

(import 'java.io.StringReader)
(defn tidy
  "Take some HTML and return a non-disgusting version of it."
  [dirty]
  (let [nodes (html-resource (StringReader. dirty))
        nodes (remove-blank-p-tags nodes)]
    (apply str
           (emit* (if-let [body (select nodes [:html :body content])]
                    body
                    nodes)))))

(def posts (map
            #(assoc % :tidied-body (tidy (post-body %)))
            all-posts))

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

;; Functions for writing out the Atom feed

(def atom-ns "http://www.w3.org/2005/Atom")
(def thread-ns "http://purl.org/syndication/thread/1.0")

;; Comments have postlevel = 2 and ParentID != PostID
(import 'java.text.SimpleDateFormat)
(def input-date-format (SimpleDateFormat. "MM/dd/yyyy HH:mm:ss"))
(def output-date-format (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.S'-05:00'"))

(defn xml-date-string
  "Convert a 12/23/2011 12:34:56 time string to an Atom-compatible string."
  [d]
  (.format output-date-format (.parse input-date-format d)))

(defn write-element-string
  "Writes an element with the specified name, namespace, and content
  to the writer."
  [writer ns name content]
  (.writeStartElement writer ns name)
  (.writeCharacters writer content)
  (.writeEndElement writer))

(defn write-post-entry
  "Given a post or comment write the entries for it to the XmlWriter."
  [writer post]
  (let [id (post-id post)
        time (post-time post)
        title (post-title post)
        body (:tidied-body post)
        author (post-author post)
        comments (:comments post)]
  (doto writer
    (.writeStartElement atom-ns "entry")
    (write-element-string atom-ns "id" (str id))
    (write-element-string atom-ns "published" (xml-date-string time))
    (write-element-string atom-ns "updated" (xml-date-string time))

    (.writeStartElement atom-ns "category")
    (.writeAttribute "scheme" "http://schemas.google.com/g/2005#kind")
    (.writeAttribute "term"
                           (if (toplevel? post)
                             "http://schemas.google.com/blogger/2008/kind#post"
                             "http://schemas.google.com/blogger/2008/kind#comment"))
    (.writeEndElement)

    (.writeStartElement atom-ns "title")
    (.writeAttribute "type" "text")
    (.writeCharacters (post-title post))
    (.writeEndElement)

    (.writeStartElement atom-ns "content")
    (.writeAttribute "type" "html")
    (.writeCharacters body)
    (.writeEndElement)

    (.writeStartElement atom-ns "author")
    (write-element-string atom-ns "name" author)
    (.writeEndElement))

  (when (and (pos? (count comments))
             (toplevel? post))
    (write-element-string writer thread-ns "total" (str (count comments))))

  (when (not (toplevel? post))
    (.writeStartElement writer thread-ns "in-reply-to")
    (.writeAttribute writer "ref" (str (post-parent post)))
    (.writeEndElement writer))

  (.writeEndElement writer)             ; </entry>

  (doseq [comment comments]
    (write-post-entry writer comment))))

(use 'clojure.java.io)
(import 'javax.xml.stream.XMLOutputFactory)
(import 'java.util.Date)


(defn write-feed
  "Spit out an Atom feed file with the specified name, given the posts."
  [path posts]
  (let [factory (XMLOutputFactory/newInstance)
        _ (.setProperty factory "javax.xml.stream.isRepairingNamespaces" true)
        writer (.createXMLStreamWriter factory (writer path))]
    (try
      (doto writer
        (.writeComment "Generated by a crappy script that Craig Andera wrote : https://github.com/candera/cs-atom")
        (.setDefaultNamespace atom-ns)
        (.setPrefix "thr" thread-ns)
        (.writeStartElement atom-ns "feed")
        (write-element-string atom-ns "id" "feed-id")
        (write-element-string atom-ns "updated" (.format output-date-format (Date.)))

        (.writeStartElement atom-ns "title")
        (.writeAttribute "type" "text")
        (.writeCharacters "Blog Title Here")
        (.writeEndElement)

        (.writeStartElement atom-ns "generator")
        (.writeAttribute "version" "7.00")
        (.writeAttribute "uri" "http://www.blogger.com")
        (.writeCharacters "Blogger")
        (.writeEndElement))

      (doseq [post posts] (write-post-entry writer post))

      (.writeEndElement writer)

      (finally
       (.flush writer)
       (.close writer)))))

(def test-content
  (->> (content-by-author "craig-andera")
       (filter #(seq (:comments %)))
       (take 3)
       (map #(assoc % :comments (take 3 (:comments %))))))

(write-feed "/tmp/craig-andera-3.atom" test-content)
