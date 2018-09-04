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


(defn divisible-by? [n d]
  (zero? (mod n d)))

(defmacro new-hfsm [inputs transitions & {:keys [init-state slaves]
                                          :or {init-state (first transitions)
                                               slaves {}}}]
  {:pre [(divisible-by? (count transitions) 3)]}
  (let [tr-map (group-by first (partition 3 transitions))]
    `{:tr-func (fn [st# {:keys ~inputs}]
                 (or (case st#
                       ~@(apply
                          concat
                          (for [[s ts] tr-map]
                            `(~s (cond
                                   ~@(apply
                                      concat
                                      (for [[_ gs os] ts]
                                        `(~gs ~os))))))))
                     [st# #{}])
                 )
      :init-st ~init-state
      :slaves ~slaves})
  )

(def hfsm3
  (new-hfsm [a b]
            [:A (and a (not b)) [:A #{:v}]
             :A (or (not a) b)  [:B #{:u :v}]
             :B a               [:B #{:u}]
             :B (and (not a) b) [:A #{}]]))

(def hfsm4
  (new-hfsm [a b]
            [:A (and a (not b)) [:A #{:v}]
             :A (or (not a) b)  [:B #{:u :v}]
             :B a               [:B #{:u}]
             :B (and (not a) b) [:A #{}]]
            :init-st :A
            :slaves
            {:B (new-hfsm [a b]
                          [:C a [:D #{:v}]
                           :D b [:C #{:v}]])}))


;; (println (macroexpand-1
;;           '(new-hfsm [a b]
;;                      [:A (and a (not b)) [:A #{:v}]
;;                       :A (or (not a) b)  [:B #{:u :v}]
;;                       :B a               [:B #{:u}]
;;                       :B (and (not a) b) [:A #{}]])))

(defn step [{:keys [tr-func slaves] :or {slaves {}}} [st & sts] js]
  (let [[new-st os] (tr-func st js)]
    ;;(println slaves st sts new-st os)
    (if-let [slave (slaves new-st)]
      (let [sl-sts (if (= st new-st) sts [(:init-st slave)])
            [sl-new-st sl-os] (step slave sl-sts js)]
        [(cons new-st sl-new-st) (clojure.set/union os sl-os)])
      [[new-st] os])))


(defn run [hfsm input-seq]
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
