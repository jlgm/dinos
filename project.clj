(defproject dinos "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] 
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.logging "0.4.1"]
                 [compojure "1.6.1"]
                 [http-kit "2.3.0"]]
;  :plugins [[lein-codox "0.10.4"]]
  :main ^:skip-aot dinos.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
