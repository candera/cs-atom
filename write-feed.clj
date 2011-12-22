;; TODO: This bit is currently written against the .NET APIs. Rewrite
;; it to use Java XML writing
(System.Reflection.Assembly/LoadWithPartialName "System.Xml")
(import '[System.Xml XmlTextWriter Formatting XmlConvert XmlDateTimeSerializationMode])
(import 'System.DateTime)

(def atom-ns "http://www.w3.org/2005/Atom")
(def thread-ns "http://purl.org/syndication/thread/1.0")

;; Comments have postlevel = 2 and ParentID != PostID

(defn xml-date-string
  "Convert a DateTime to an Atom-compatible string."
  [date]
  (XmlConvert/ToString date XmlDateTimeSerializationMode/Utc))

(defn write-post-entry
  "Given a post or comment write the entries for it to the XmlWriter."
  [writer post]
  (let [id (post-id post)
        time (post-time post)
        title (post-title post)
        body (post-body post)
        author (post-author post)
        comments (:comments post)]
  (doto writer
    (.WriteStartElement "entry" atom-ns)
    (.WriteElementString "id" atom-ns (str id))
    (.WriteElementString "published" atom-ns (xml-date-string time))
    (.WriteElementString "updated" atom-ns (xml-date-string time))

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
    (.WriteEndElement)

    (.WriteStartElement "content" atom-ns)
    (.WriteAttributeString "type" "html")
    (.WriteValue (post-body post))
    (.WriteEndElement)

    (.WriteStartElement "author" atom-ns)
    (.WriteElementString "name" atom-ns author)
    (.WriteEndElement))

  (when (and (pos? (count comments))
             (toplevel? post))
    (.WriteElementString writer "total" thread-ns (str (count comments))))

  (when (not (toplevel? post))
    (.WriteStartElement writer "in-reply-to" thread-ns)
    (.WriteAttributeString writer "ref" (str (post-parent post)))
    (.WriteEndElement writer))

  (.WriteEndElement writer)             ; </entry>

  (doseq [comment comments]
    (write-post-entry writer comment))))


(defn write-feed
  "Spit out an Atom feed file with the specified name, given the posts."
  [path posts]
  (let [writer (XmlTextWriter. path nil)]
    (try
      (doto writer
        (.set_Formatting Formatting/Indented)
        (.WriteStartElement "feed" atom-ns)
        (.WriteElementString "id" atom-ns "feed-id")
        (.WriteElementString "updated" atom-ns (xml-date-string (DateTime/Now)))

        (.WriteStartElement "title" atom-ns)
        (.WriteAttributeString "type" "text")
        (.WriteValue "Blog Title Here")
        (.WriteEndElement)

        (.WriteStartElement "generator" atom-ns)
        (.WriteAttributeString "version" "7.00")
        (.WriteAttributeString "uri" "http://www.blogger.com")
        (.WriteValue "Blogger")
        (.WriteEndElement))

      (doseq [post posts] (write-post-entry writer post))

      (finally
       (.Close writer)))))

(def test-content
  (->> (content-by-author "craig-andera")
       (filter #(seq (:comments %)))
       (take 3)
       (map #(assoc % :comments (take 3 (:comments %))))))

(write-feed "C:\\temp\\craig-andera-3.atom" test-content)
