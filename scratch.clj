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

(def posts (let [conn (SqlConnection. "Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CommunityServer;Data Source=.")
                 _ (.Open conn)]
             (select conn "select * from dbo.cs_Posts")))

(def threads (let [conn (SqlConnection. "Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CommunityServer;Data Source=.")
                 _ (.Open conn)]
             (select conn "select * from dbo.cs_Threads")))


(def authors (distinct (map #(get % "PostAuthor") threads)))

(def threads-by-author (group-by #(get % "PostAuthor") threads))

(defn posts-for-thread
  "Filter a seq of posts for a given thread."
  [posts thread]
  (filter #(= (get thread "ThreadID") (get % "ThreadID")) posts))

(->> (threads-by-author "craig-andera")
     first
     (posts-for-thread posts)
     (take 5)
     pprint
     )

(pprint (threads-by-author "craig-andera"))