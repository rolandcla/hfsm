(ns hfsm.valued-fsm)

(def hfsm1
  {:tr-func (fn [st {:keys [a b]}]
              (or (case st
                    :A (cond (and a (not b)) [:A [(fn [os] (-> os (conj :v) (disj :u)))]]
                             (or (not a) b)  [:B [(fn [os] (conj os :u :v))]])
                    :B (cond a               [:B [(fn [os] (-> os (conj :u) (disj :v)))]]
                             (and (not a) b) [:A [(fn [os] (disj os :u :v))]]))
                  [st []]))
   :init-st :A
   :slaves {}})

(def hfsm2
  {:tr-func (fn [st {:keys [a]}]
              (or (case st
                    :A (cond (= 1 a) [:B []])
                    :B (cond (= 1 a) [:C []]
                             :else   [:A []])
                    :C (cond (= 1 a) [:D [(fn [os] (-> os (assoc :o 42) (update :cnt inc)))]]
                             :else   [:A []])
                    :D (cond (= 0 a) [:A [(fn [os] (assoc os :o 0))]]))
                  [st []]))
   :init-st :A})

(defn divisible-by? [n d]
  (zero? (mod n d)))

(defmacro new-hfsm [inputs transitions & {:keys [init-st slaves]
                                          :or {init-st (first transitions)
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
                     [st# []])
                 )
      :init-st ~init-st
      :slaves ~slaves}))

(def hfsm3
  (new-hfsm [a]
            [:A (= 1 a) [:B []]
             :B (= 1 a) [:C []]
             :B :else   [:A []]
             ;;:C (= 1 a) [:D [#(-> %1 (assoc :o 42) (update :cnt inc))]]
             :C (= 1 a) [:D [#(assoc %1 :o 42) #(update %1 :cnt inc)]]
             :C :else   [:A []]
             :D (= 0 a) [:A [#(assoc %1 :o 0)]]
             ]
            :init-st :D
            :slaves
            {:D (new-hfsm [a]
                          [:X true [:Y []]
                           :Y true [:X [(fn [os] (update os :cnt1 #(inc (or %1 0))))]]])}
            ))

(defn step [{:keys [tr-func slaves] :or {slaves {}}}
            [st & sts]
            js
            os]
  (let [[new-st os-fns] (tr-func st js)
        new-os (reduce #(%2 %1) os os-fns)]
    (if-let [slave (slaves new-st)]
      (let [sl-sts (if (= st new-st) sts [(:init-st slave)])
            [sl-new-st sl-os] (step slave sl-sts js new-os)]
        [(cons new-st sl-new-st) sl-os])
      [[new-st] new-os])))


(defn run [hfsm os input-seq]
  (reduce (fn [[st os] js]
            (let [[new-st new-os] (step hfsm st js os)]
              (println new-st new-os)
              [new-st new-os]))
          [[(:init-st hfsm)] os]
          input-seq))

