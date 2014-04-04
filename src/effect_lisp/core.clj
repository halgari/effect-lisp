(ns effect-lisp.core
  (:refer-clojure :exclude [eval]))


(defprotocol IObjectSpace
  (invoke [this fn args cont] "Returns a [val env] pair")
  (ground [this form cont] "Returns a [val env] pair"))

(defrecord Thunk [data])
(defrecord Answer [data])

(defn thunk [& args]
  (->Thunk args))

(declare interpret)

(defn interpret-seq [completed remaining cont]
  (if remaining
    (interpret (first remaining)
               (fn [grounded]
                 (interpret-seq (conj completed grounded)
                                (next remaining)
                                cont)))
    (thunk invoke
           (first completed)
           (next completed)
           cont)))

(defn interpret [form cont]
  (cond
   (seq? form) (interpret-seq [] form cont)
   :else (thunk ground form cont)))



(defn eval-loop [form objspace]
  (println "---")
  (loop [objspace objspace
         cont (interpret form ->Answer)]
    (println cont (instance? Answer cont))
    (cond
     (instance? Answer cont)
     [(:data cont) objspace]

     (ifn? cont) (recur objspace (cont))
     
     (instance? Thunk cont)
     (let [[f & data] (:data cont)
           [val cont objspace] (apply f objspace data)]
       (recur objspace (cont val)))

     :else (assert false (str (type cont) cont)))))



(defrecord InterpObjectSpace []
  IObjectSpace
  (ground [this x cont]
    (cond
     (integer? x) [x cont this]
     (symbol? x) [+ cont this]
     :else (assert false (str "Can't ground " (pr-str x)))))
  (invoke [this fn args cont]
    [(apply fn args)
     cont
     this]))

(defn gsym [state nm]
  (symbol (str nm "_" (swap! state inc))))

(defrecord TaggedData [data meta])

(defn tag [data meta]
  (->TaggedData data meta))

(defrecord TracingObjectSpace [trace interp sym]
  IObjectSpace
  (ground [this x cont]
    (let [[v c state] (ground interp x cont)
          sym (gsym sym "const")]
      [(tag v sym)
       c
       (assoc this
         :interp state
         :trace (conj trace [sym :const x]))]))
  (invoke [this f args cont]
    (let [[v c state] (invoke interp (:data f) (map :data args) cont)
          sym (gsym sym "invoke")]
      [(tag v sym)
       c
       (assoc this
         :interp state
         :trace (conj trace [sym :invoke (:meta f) (map :meta args)]))])))

(clojure.pprint/pprint (eval-loop '(+ 1 (+ (+ 3 3) 2)) (->TracingObjectSpace [] (->InterpObjectSpace) (atom 0))))
