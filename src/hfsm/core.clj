(ns hfsm.core
  (:gen-class))

(def hfsm1
  {:tr-func (fn [st js]
              (or (case st
                    :A (cond (and (:a js) (not (:b js))) [:A #{:v}]
                             (or (not (:a js)) (:b js))  [:B #{:u :v}])
                    :B (cond (:a js)                     [:B #{:u}]
                             (and (not (:a js)) (:b js)) [:A #{}]))
                  [st #{}]))
   :init-st :A})

(def hfsm2
  {:tr-func (fn [st js]
              (or (case st
                    :A (cond (and (:a js) (not (:b js))) [:A #{:v}]
                             (or (not (:a js)) (:b js))  [:B #{:u :v}])
                    :B (cond (:a js)                     [:B #{:u}]
                             (and (not (:a js)) (:b js)) [:A #{}]))
                  [st #{}]))
   :init-st :A
   :slaves
   {:B {:tr-func (fn [st {:keys [a b]}]
                   (or (case st
                         :C (cond a [:D #{:v}])
                         :D (cond b [:C #{:v}]))
                       [st #{}]))
        :init-st :C
        :slaves {}}}})

(defn run1 [{:keys [tr-func init-st]} input-seq]
  (reduce
   (fn [old-st js]
     (let [[st os] (tr-func old-st js)]
       (println st os)
       st))
   init-st
   input-seq))

(defn run2 [{:keys [tr-func init-st]} input-seq]
  (reduce
   (fn [old-st js]
     (let [[st os] (tr-func (first old-st) js)]
       (println st os)
       [st]))
   [init-st]
   input-seq))

(defn run3
  ([hfsm input-seq]
   (run3 hfsm (:init-st hfsm) input-seq))
  ([{:keys [tr-func slaves]} init-st input-seq]
   ;;(println "run3" init-st input-seq)
   (reduce
    (fn [[old-st & old-sts] js]
      (let [[st os] (tr-func old-st js)]
        (println st os)
        (if-let [slave (slaves st)]
          (cons st
                (if (= st old-st)
                  (run3 slave (first old-sts) [js])
                  (run3 slave [js])))
          [st])))
    [init-st]
    input-seq)))

(defn step [{:keys [tr-func slaves]} [st & sts] js]
  (let [[new-st os] (tr-func st js)]
    (if-let [slave (slaves new-st)]
      (let [sl-sts (if (= st new-st) sts [(:init-st slave)])
            [sl-new-st sl-os] (step slave sl-sts js)]
        [(cons new-st sl-new-st) (clojure.set/union os sl-os)])
      [[new-st] os])))


(defn run4 [hfsm input-seq]
  (reduce (fn [st js]
            (let [[new-st os] (step hfsm st js)]
              (println new-st os)
              new-st))
          [(:init-st hfsm)]
          input-seq))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
