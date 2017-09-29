;; Original work
;; by Joel Boehland http://github.com/jolby/rincanter
;; January 24, 2010

;; Copyright (c) Joel Boehland, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; Modified work by svarcheg https://github.com/svarcheg/rincanter
;; May 5, 2015

(ns rincanter.core-test
  (:require [clojure.core.matrix :refer [matrix matrix?]]
            clojure.core.matrix.dataset
            [clojure.test :refer :all]
            [incanter.core :refer [$ dataset dim with-data]]
            [incanter.stats :refer [mean within]]
            [rincanter.convert :refer [from-r r-attr r-true to-r]]
            [rincanter.core :refer :all])
  (:import [org.rosuda.REngine REXPDouble REXPInteger REXPLogical REXPString]
           org.rosuda.REngine.Rserve.RConnection))

;;taken from incanter information_theory_tests.clj
(def ^:dynamic *R* nil)

(defn r-connection-fixture [test-fn]
  (let [p (:process (start-rserve))]
    (Thread/sleep 1000)                                     ;; allow for server to boot!
     (binding [*R* (get-r)]
           (test-fn)
           (.close ^RConnection *R*))
    (.destroy p)))

(deftest can-connect-to-R
  (is (not (= nil *R*))))

(deftest to-r-conversions
  (is (= REXPLogical (class (to-r (into-array Byte/TYPE (map #'byte [1 2 3]))))))
  (is (= REXPInteger (class (to-r (into-array Integer/TYPE [1 2 3])))))
  (is (= REXPDouble (class (to-r (into-array Double/TYPE [1.0 2.0 3.0])))))
  (is (= REXPString (class (to-r (into-array String ["fee" "fie" "foe"])))))
  ;;test types with meta data hints set
  (is (= REXPLogical (class (to-r (with-meta [1 2 3] {:r-type REXPLogical})))))
  (is (= REXPInteger (class (to-r (with-meta [1 2 3] {:r-type REXPInteger})))))
  (is (= REXPDouble (class (to-r (with-meta [1 2 3] {:r-type REXPDouble})))))
  (is (= REXPDouble (class (to-r (with-meta [1.0 2.0 3.0] {:r-type REXPDouble})))))
  (is (= REXPString (class (to-r (with-meta ["fee" "fie" "foe"] {:r-type REXPString})))))
  ;;seq conversions
  (is (= REXPInteger (class (to-r [1 2 3]))))
  (is (= REXPDouble (class (to-r [1.9 2.0 3.9]))))
  #_(is (= (dataset ["c1" "c2"] '((1 2) (3 4))) (from-r (to-r (dataset ["c1" "c2"] '((1 2) (3 4))))))))

(deftest pass-through-int-vector
  (r-set! *R* "iv1" (to-r [1 2 3]))
  (is (= [1 2 3] (r-get *R* "iv1"))))

;(deftest from-r-int-vector
;  (r-eval *R* "iv2 = c(1, 2, 3)")
;  (is (= [1 2 3] (r-get *R* "iv2"))))

(deftest from-r-double
  (r-eval *R* "x = 42")
  (is (= [42.0] (r-get *R* "x"))))

(deftest from-r-list
  (let [l (r-eval *R* "list(1,2,3)")]
    (is (= [[1.0] [2.0] [3.0]] l))))

(deftest from-r-named-list
  (r-eval *R* "nl = list(foo=2,bar=3)")
  (let [arraymap (array-map "foo" [2.0] "bar" [3.0])
        nl (r-get *R* "nl")]
    (is (= arraymap nl))))

(deftest pass-through-double-vector
  (r-set! *R* "dv1" (to-r [1.0 2.0 3.0]))
  (is (= [1.0 2.0 3.0] (r-get *R* "dv1"))))

(deftest from-r-double-vector
  (r-eval *R* "dv2 = c(1.0, 2.0, 3.0)")
  (is (= [1.0 2.0 3.0] (r-get *R* "dv2"))))

(deftest convert-dataframe-to-dataset
  (with-r-eval
    "data(iris)"
    ;;starts off an R dataframe, turns into an incanter dataset
    (is (= (type (r-get *R* "iris")) clojure.core.matrix.impl.dataset.DataSet)))) ;TODO fix class

(deftest dataframe-dataset-dim-equivalence
  (is (= [150 5] (r-eval *R* "dim(iris)")))
  (is (= [150 5] (dim (r-get *R* "iris")))))

(deftest metadata
  (let [iris-dataframe (r-eval-raw *R* "iris")
        names (from-r (r-attr iris-dataframe "names"))
        cols (map from-r (.asList iris-dataframe))
        col-meta (zipmap names (map meta cols))]
    (is (= (get-in col-meta ["Species" :category-variable :labels])
           ["setosa" "versicolor" "virginica"]))))

(deftest factors
  (let [iris (r-eval *R* "iris")]
    (is (=  (get (first (clojure.core.matrix.dataset/row-maps iris)) "Sepal.Length") 5.1))))

(deftest pass-through-dataframe-equivalence
  (with-r-eval
    "data(iris)"
    ;;convert iris dataframe to an incanter dataset, then convert back
    ;;to an R dataframe and set it in the R environment
    (r-set! *R* "irisds" (to-r (r-get *R* "iris")))
    ;;irisds is now an R dataframe it should be identical to iris dataframe
    (is (r-true (r-eval *R* "identical(irisds, iris)")))))

(deftest dataframe-dataset-mean
  (with-data (r-get *R* "iris")
    (is (within 0.000001
                (mean ($ "Sepal.Width"))
                 ((r-eval *R* "mean(iris$Sepal.Width)") 0)))))



(deftest get-matrix
   "get matrix from R"
  (let [m (r-get *R* "matrix(c(1,2,3,4,5,6),nrow=2)")]
    (is (matrix? m))
    (is (= [2 3] (dim m)))
    )
  ) 

(deftest set-matrix
  (r-set! *R* "mat"  (clojure.core.matrix/matrix [[1 2 3] [4 5 6]]))
  (is (= ["matrix"] (r-eval *R* "class(mat)")))
  (is (= [2 3] (r-eval *R* "dim(mat)")))
  (is (= [ 1.0 4.0 2.0 5.0 3.0 6.0] (r-eval *R* "as.vector(mat)"))))

(deftest set-matrix-eigen
  (r-set! *R* "mat"  (matrix [[1 2] [ 3 4]]))
  (r-void-eval *R* "eig=eigen(mat)")
  (is (= [5.372281323269014 -0.3722813232690143] (get (r-get *R* "eig") "values"))))



(use-fixtures :once r-connection-fixture)

