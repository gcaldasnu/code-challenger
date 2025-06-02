(defproject challenger-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :repositories ^:replace [["nu-codeartifact" {:url "https://maven.cicd.nubank.world"}]]

  :dependencies [[org.clojure/clojure "1.11.1"] [org.clojure/test.check "1.1.1"]]
  :repl-options {:init-ns challenger-clojure.core})
