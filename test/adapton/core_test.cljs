(ns adapton.core-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [adapton.core :as ad :refer-macros [adapt defavar avar avar-set! defamemo]]))

(deftest test-aref
  (let [r1 (ad/aref 8)
        r2 (ad/aref 10)
        a (ad/make-athunk nil)]
    (set! (.-thunk a) (fn []
                        (ad/+edge! a r1)
                        (ad/+edge! a r2)
                        (- (ad/compute r1)
                           (ad/compute r2))))
    (is (== -2 (ad/compute a)))
    (ad/aref-set! r1 2)
    (is (== -8 (ad/compute a)))))

(deftest test-adapton?
  (is (= true (ad/adapton? (ad/aref 5))))
  (is (= false (ad/adapton? 5))))

(deftest test-aforce
  (let [r (ad/aref 5)
        a (ad/make-athunk #(+ (ad/aforce r) 3))]
    (is (= 8 (ad/aforce a)))
    (ad/aref-set! r 2)
    (is (= 5 (ad/aforce a)))))

(deftest test-adapt
  (let [r1 (ad/aref 2)
        r2 (ad/aref (+ (ad/aforce r1) 4))
        a (adapt (+ (ad/aforce r1)
                    (ad/aforce r2)))]
    (is (= 8 (ad/aforce a)))
    (ad/aref-set! r1 10)
    (is (= 16 (ad/aforce a)))))

(deftest test-defavar
  (defavar v1 2)
  (defavar v2 (+ (ad/avar-get v1) 4))
  (defavar b (+ (ad/avar-get v1) (ad/avar-get v2)))
  (is (= 8 (ad/avar-get b)))
  (avar-set! v1 10)
  (is (= 24 (ad/avar-get b))))

(deftest test-avar
  (let [v1 (avar 2)
        v2 (avar (+ (ad/avar-get v1) 4))
        b (avar (+ (ad/avar-get v1) (ad/avar-get v2)))]
    (is (= 8 (ad/avar-get b)))
    (avar-set! v1 10)
    (is (= 24 (ad/avar-get b)))))

(deftest test-tree
  (defavar lucky 7)
  (defavar t1 [1 2])
  (defavar t2 [3 4])
  (defavar some-tree [(ad/avar-get t1) (ad/avar-get t2)])

  (defamemo max-tree [t]
    (cond
      (ad/adapton? t) (recur (ad/aforce t))
      (coll? t) (max (max-tree (first t))
                     (max-tree (second t)))
      :else t))

  (defamemo max-tree-path [t]
    (cond
      (ad/adapton? t) (recur (ad/aforce t))
      (coll? t) (if (> (max-tree (first t))
                       (max-tree (second t)))
                  (cons 'left (max-tree-path (first t)))
                  (cons 'right (max-tree-path (second t))))
      :else []))

  (is (= [[1 2] [3 4]] (ad/avar-get some-tree)))
  (is (= 4 (max-tree some-tree)))
  (is (= '[right right] (max-tree-path some-tree)))

  (avar-set! t2 5)
  (is (= [[1 2] 5] (ad/avar-get some-tree)))
  (is (= 5 (max-tree some-tree)))
  (is (= '[right] (max-tree-path some-tree)))
  (is (= 5 (max-tree (second (ad/avar-get some-tree)))))
  (is (= [] (max-tree-path (second (ad/avar-get some-tree)))))

  (avar-set! t2 (vector 20 (* 3 (ad/avar-get lucky))))
  (is (= [[1 2] [20 21]] (ad/avar-get some-tree)))
  (is (= 21 (max-tree some-tree)))
  (is (= '[right right] (max-tree-path some-tree)))

  (avar-set! lucky 3)
  (is (= [[1 2] [20 9]] (ad/avar-get some-tree)))
  (is (= 20 (max-tree some-tree)))
  (is (= '[right left] (max-tree-path some-tree))))

(deftest test-html
  (def data
    {"4" ["5" "9"],
     "0" ["8"],
     "1" ["2" "9"],
     "2" ["4" "8"],
     "7" ["7"],
     "9" ["7" "1" "2" "8"],
     "8" ["0" "7"],
     "5" ["0"]})

  (def avar-data
    (reduce-kv
      (fn [ret k v]
        (assoc ret k (avar v)))
      {}
      data))

  (defn hiccup [data ks max-depth]
    (when-not (zero? max-depth)
      (into [:ul] (for [k ks]
                    (if-let [d (hiccup data (get data k) (dec max-depth))]
                      [:li k d]
                      [:li k])))))

  (declare hiccup-memo)

  (defn hiccup-memo* [data ks max-depth]
    (when-not (zero? max-depth)
      (into [:ul]
            (for [k ks]
              (if-let [d (hiccup-memo data (get data k) (dec max-depth))]
                [:li k d]
                [:li k])))))

  (def hiccup-memo (memoize hiccup-memo*))

  (defamemo hiccup-amemo [data ks max-depth]
    (cond
      (ad/adapton? ks) (hiccup-amemo data (ad/aforce ks) max-depth)
      (zero? max-depth) nil
      :else (into [:ul]
                  (for [k ks]
                    (if-let [d (hiccup-amemo data (get data k) (dec max-depth))]
                      [:li k d]
                      [:li k])))))

  (simple-benchmark [] (count (hiccup data ["1"] 20)) 100)
  (simple-benchmark [] (count (hiccup-memo data ["1"] 20)) 100)
  (simple-benchmark [] (count (hiccup-amemo avar-data ["1"] 20)) 100)

  (is (= (hiccup       data      ["1"] 20)
         (hiccup-memo  data      ["1"] 20)
         (hiccup-amemo avar-data ["1"] 20))))

(defn -main [& args]
  (run-tests))
