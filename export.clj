;; This stuff needs to run under ClojureCLR

(System.Reflection.Assembly/LoadWithPartialName "System.Data")
(import '[System.Data.SqlClient SqlConnection SqlCommand])

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
    (select conn "select PostID, ParentID, Body, FormattedBody, IsApproved, PostAuthor, PostType, PostName, PostDate, Subject, PostLevel from dbo.cs_Posts")))

(let [writer (System.IO.StreamWriter. "raw-data.clj")]
  (binding [*out* writer]
    (pprint all-posts))
  (.Close writer))