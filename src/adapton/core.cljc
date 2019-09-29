(ns adapton.core
  #?(:cljs (:require-macros [adapton.core :refer [adapt]])))

(def ^:private curr-adapting (volatile! false))

(defprotocol IAdapton
  (+edge! [this a-sub])
  (-edge! [this a-sub])
  (compute [this])
  (dirty! [this])
  (aforce [this])
  (set-thunk! [this new-thunk])
  (set-result! [this new-result])
  (get-sup [this])
  (set-sup! [this new-sup])
  (get-result [this]))

(deftype Adapton #?(:cljs [^:mutable thunk
                           ^:mutable result
                           ^:mutable sub
                           ^:mutable sup
                           ^:mutable clean?]
                    :clj [^:volatile-mutable thunk
                          ^:volatile-mutable result
                          ^:volatile-mutable sub
                          ^:volatile-mutable sup
                          ^:volatile-mutable clean?])
  IAdapton
  (get-sup [this]
    sup)
  (set-sup! [this new-sup]
    (set! sup new-sup))
  (get-result [this]
    result)
  #?(:cljs (+edge! [this a-sub]
             (.add sub a-sub)
             (.add (.-sup a-sub) this))
     :clj (+edge! [this a-sub]
            (set! sub (conj sub a-sub))
            (set-sup! a-sub (conj (get-sup a-sub) this))))
  #?(:cljs (-edge! [this a-sub]
                   (.delete sub a-sub)
                   (.delete (get-sup a-sub) this))
     :clj (-edge! [this a-sub]
            (set! sub (disj sub a-sub))
            (set-sup! a-sub (disj (get-sup a-sub) this))))
  (compute [this]
    (if clean?
      result
      (do
        #?(:cljs (-> (.from js/Array sub) (.forEach #(-edge! this %)))
           :clj (run! #(-edge! this %) sub))
        (set! clean? true)
        (set! result (thunk))
        (recur))))
  (dirty! [this]
    (when clean?
      (set! clean? false)
      #?(:cljs (-> (.from js/Array sup) (.forEach #(dirty! %)))
         :clj (run! dirty! sup))))
  (aforce [this]
    (let [prev-adapting (volatile! @curr-adapting)
          _ (vreset! curr-adapting this)
          result (compute this)
          _ (vreset! curr-adapting @prev-adapting)]
      (when @curr-adapting
        (+edge! @curr-adapting this))
      result))
  (set-thunk! [this new-thunk]
    (set! thunk new-thunk))
  (set-result! [this new-result]
    (set! result new-result)))

(defn adapton? [v]
  (instance? Adapton v))

(defn make-athunk [thunk]
  #?(:cljs (Adapton. thunk nil (js/Set.) (js/Set.) false)
     :clj (Adapton. thunk nil #{} #{} false)))

(defn aref [v]
  (let [a #?(:cljs (Adapton. nil v (js/Set.) (js/Set.) true)
             :clj (Adapton. nil v #{} #{} true))]
    (set-thunk! a #(get-result a))
    a))

(defn aref-set! [a v]
  (set-result! a v)
  (dirty! a))

#?(:clj
    (defmacro adapt [e]
      `(make-athunk (fn [] ~e))))

(defn amemoize [f]
  (let [f* (memoize #(adapt (apply f %&)))]
    #(aforce (apply f* %))))

(defn avar-get [v]
  (aforce (aforce v)))

#?(:clj
   (do
     (defmacro amemo [args & body]
       `(let [f*# (amemoize (fn ~args ~@body))]
          (fn [& args#] (f*# args#))))

     (defmacro defamemo [f args & body]
       `(def ~f (amemo ~args ~@body)))

     (defmacro avar [e]
       `(aref (adapt ~e)))

     (defmacro defavar [name e]
       `(def ~name (avar ~e)))

     (defmacro avar-set! [v e]
       `(aref-set! ~v (adapt ~e)))))
