(defproject rojure
  "0.1.0-SNAPSHOT"
  :description "Clojure/R integration using rosuda Rserve"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.4.0"]
                 [org.rosuda.REngine/REngine "2.1.0"]
                 [org.clojure/core.incubator "0.1.4"]
                 [org.rosuda.REngine/Rserve "1.8.1"]
                 ;[net.mikera/core.matrix "0.61.0"] ; todo: not working
                 [net.mikera/core.matrix "0.52.0" :exclusions [org.clojure/clojure]]
                 ]

  :profiles {:deployer {:repositories [["snapshots" {:url "file:///var/go/mvn-repo/"
                                                     :sign-releases false}]
                                       ["releases" {:url "file:///var/go/mvn-repo/"
                                                    :creds nil
                                                    :sign-releases false}]]}}
  :plugins [[lein-kibit "0.1.2"]
            [lein-ancient "0.6.7"]
            [lein-bikeshed "0.2.0"]
            [lein-cloverage "1.0.3"]
            [codox "0.8.12"]])
