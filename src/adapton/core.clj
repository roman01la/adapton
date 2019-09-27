(ns adapton.core)

(defmacro adapt [e]
  `(make-athunk (fn [] ~e)))

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
  `(aref-set! ~v (adapt ~e)))
