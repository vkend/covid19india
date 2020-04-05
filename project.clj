(defproject india-covid-extractor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [pdfboxing "0.1.15-SNAPSHOT"]
				 [com.github.kyleburton/clj-xpath "1.4.11"]
				 [hickory "0.7.1"]
				 [cheshire "5.10.0"]]
  :resource-paths ["resources/Siebel.jar" "resources/traprange.lastest.jar" "resources/pdfextractor-0.0.1-SNAPSHOT.jar"]
  :main ^:skip-aot india-covid-extractor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
