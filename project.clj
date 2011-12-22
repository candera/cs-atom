(defproject cs-atom "0.1.0-SNAPSHOT"
  :description "Some scripts for getting data out of a CommunityServer
  SQL database and into an Atom feed."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [enlive "1.0.0"]]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]]
  :jvm-opts ["-server" "-Xmx1g"])
