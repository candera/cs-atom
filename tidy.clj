(import 'java.io.PushbackReader)
(use 'clojure.java.io)
(def all-posts
  (let [source (PushbackReader. (reader "raw-data.clj"))]
    (loop [posts []]
      (let [post (read source false :done)]
        (if (= post :done)
          posts
          (recur
           (if (post "IsApproved")
             (conj posts post)
             posts)))))))

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

(def posts all-posts)

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
