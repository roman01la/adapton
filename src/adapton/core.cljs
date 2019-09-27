(ns adapton.core
  (:require-macros [adapton.core :refer [adapt]]))

(def ^:dynamic *curr-adapting* false)

(defprotocol IAdapton
  (+edge! [this a-sub])
  (-edge! [this a-sub])
  (compute [this])
  (dirty! [this])
  (aforce [this]))

(deftype Adapton [^:mutable thunk
                  ^:mutable result
                  ^:mutable sub
                  ^:mutable sup
                  ^:mutable clean?]
  IAdapton
  (+edge! [this a-sub]
    (.add sub a-sub)
    (.add (.-sup a-sub) this))
  (-edge! [this a-sub]
    (.delete sub a-sub)
    (.delete (.-sup a-sub) this))
  (compute [this]
    (if ^boolean clean?
      result
      (do
        (-> (.from js/Array sub)
            (.forEach #(-edge! ^not-native this %)))
        (set! clean? true)
        (set! result (thunk))
        (recur))))
  (dirty! [this]
    (when ^boolean clean?
      (set! clean? false)
      (-> (.from js/Array sup)
          (.forEach #(dirty! ^not-native %)))))
  (aforce [this]
    (let [prev-adapting (volatile! *curr-adapting*)
          _ (set! *curr-adapting* this)
          result (compute ^not-native this)
          _ (set! *curr-adapting* @prev-adapting)]
      (when *curr-adapting*
        (+edge! ^not-native *curr-adapting* this))
      result)))

(defn adapton? [v]
  (instance? Adapton v))

(defn make-athunk [thunk]
  (Adapton. thunk nil (js/Set.) (js/Set.) false))

(defn aref [v]
  (let [a (Adapton. nil v (js/Set.) (js/Set.) true)]
    (set! (.-thunk a) #(.-result a))
    a))

(defn aref-set! [a v]
  (set! (.-result a) v)
  (dirty! ^not-native a))

(defn amemoize [f]
  (let [f* (memoize #(adapt (apply f %&)))]
    #(aforce (apply f* %))))

(defn avar-get [v]
  (aforce (aforce v)))
