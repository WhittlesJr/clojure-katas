(ns clojure-katas.potatoes.clj

  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def die-faces 6)

(def event-cfgs
  {:die-faces die-faces
   :table     [{:text      "In the Garden..."
                :die-faces die-faces
                :table     [{:text   "You happily root about all day in your garden."
                             :change {:potato 1}}
                            {:text   "You narrowly avoid a visitor by hiding in a potato sack"
                             :change {:potato  1
                                      :destiny 1}}
                            {:text   "A hooded stranger lingers outside your farm"
                             :change {:destiny 1
                                      :orc     1}}
                            {:text   "Your field is ravaged in the night by unseen enemies"
                             :change {:orc    1
                                      :potato -1}}
                            {:text   "You trade potatoes for other delicious foodstuffs"
                             :change {:potato -1}}
                            {:text   "You burrow into a bumper crop of potatoes. Do you cry with joy? Possibly."
                             :change {:potato 2}}] }
               {:text      "A Knock at the Door..."
                :die-faces die-faces
                :table     [{:text   "A distant cousin. They are after your potatoes. They may snitch on you."
                             :change {:potato 1}}
                            {:text   "A dwarven stranger. You refuse them entry. Ghastly creatures."
                             :change {:potato  1
                                      :destiny 1}}
                            {:text   "A wizard strolls by. You pointedly draw the curtains."
                             :change {:destiny 1
                                      :orc     1}}
                            {:text   "There are rumors of war in the reaches. You eat some potatoes."
                             :change {:potato -1
                                      :orc    2}}
                            {:text   "It's an elf. They are not serious people."
                             :change {:destiny 1}}
                            {:text   "It's a sack of potatoes from a generous neighbor. You really must remember to pay them a visit one of these years."
                             :change {:potato 2}}]}
               {:text   "The world becomes a darker, more dangerous place."
                :change {:orc-potato-cost 1}}]})

(def end-conditions
  [{:text    "An interfering bard or wizard turns up at your doorstep with a quest, and you are whisked away against your will on an adventure"
    :destiny 10}
   {:text   "You have enough potatoes that you can go underground and not return to the surface until the danger is past. You nestle down into your burrow and enjoy your well-earned rest."
    :potato 10}
   {:text "Orcs finally find your potato farm. Alas, orcs are not so interested in potatoes as they are in eating you, and you end up in a cookpot."
    :orc  10}])

(def initial-state
  {:orc-potato-cost 1
   :orc             0
   :potato          0
   :destiny         0
   :event-log       []})

(def stat-keys #{:orc :destiny :potato :orc-potato-cost})

(defn hurling-in-the-back-garden
  [{:keys [orc-potato-cost potato] :as state}]
  (if (>= potato orc-potato-cost)
    (-> state
        (update :potato #(- % orc-potato-cost))
        (update :orc dec))
    state))

(def midgame-conditions
  [{:text      "You remove 1 ORC score by spending potatoes"
    :orc       9
    :change-fn hurling-in-the-back-garden}])


(defn met-condition [state condition]
  (when (set/subset? (-> condition (select-keys stat-keys) (set)) (set state))
    condition))

(defn any-met-condition [state conditions]
  (->> conditions
       (map #(met-condition state %))
       (filter some?)
       (first)))

(defn lookup-roll-in-table [{:keys [table die-faces] :as _event-cfg} roll]
  (let [possible-rolls-per-event (/ die-faces (count table))
        table-index              (-> roll (dec) (/ possible-rolls-per-event ) (int))]
    (get table table-index)))

(defn build-possible-event-tree [{:keys [die-faces] :as event-cfg}]
  (let [possible-rolls (range 1 (inc die-faces))]
    (reduce (fn [event-tree roll]
              (let [matching-event (lookup-roll-in-table event-cfg roll)]
                (-> {:this matching-event
                     :roll roll}
                    (cond-> (:table matching-event) (assoc :next (build-possible-event-tree matching-event)))
                    (cons event-tree))
                ))
            []
            possible-rolls)))

(def possible-event-tree (build-possible-event-tree event-cfgs))

(defn apply-change [state change]
  (merge-with + state change))

(defn apply-event-change [state {:keys [change change-fn] :as _event-cfg}]
  (-> state
      (cond-> change    (apply-change change)
              change-fn change-fn)))

(defn stat-str [state-before event-cfg stat-key]
  (let [label       (->  stat-key name str/upper-case)
        state-after (apply-event-change state-before event-cfg)
        stat-before (get state-before stat-key)
        stat-after  (get state-after stat-key)
        value-str   (if-not (= stat-before stat-after)
                      (str stat-before "->" stat-after)
                      stat-before)]
    (str label ": " value-str)))

(defn event-log-str [state roll {:keys [text change change-fn] :as event-cfg}]
  (let [stat-strs (->> stat-keys
                       (map #(stat-str state event-cfg %))
                       (str/join "; "))]
    (str (when roll (str "ROLL: " roll "; "))
         text
         (when (or change change-fn)
           (str ".\n" stat-strs "\n")))))

(defn resolve-event-stats [state {:keys [next this roll] :as _event-node}]
  (-> state
      (update :event-log conj (event-log-str state roll this))
      (apply-event-change this)
      (cond-> next       (resolve-event-stats (rand-nth next)))))

(defn play []
  (loop [state initial-state]
    (let [new-state             (resolve-event-stats state (rand-nth possible-event-tree))
          met-end-condition     (any-met-condition new-state end-conditions)
          met-midgame-condition (any-met-condition new-state midgame-conditions)]
      (cond met-end-condition     (update new-state :event-log conj (str "GAME OVER: " (:text met-end-condition)))
            met-midgame-condition (-> new-state
                                      (resolve-event-stats met-midgame-condition)
                                      (recur))
            :else                 (recur new-state)))))



(->> (play)
     :event-log
     (str/join "\n")
     (println))



;; Event config validation, not part of the game itself

(defn event-cfg-valid? [{:keys [die-faces table change] :as event-cfg}]
  (or (some? change)
      (some #(contains? event-cfg %) stat-keys)
      (= (mod die-faces (count table))
         0)))

(defn validate-event-cfgs [{:keys [table] :as event-cfg}]
  (assert (event-cfg-valid? event-cfg))
  (when table
    (map validate-event-cfgs table)))

(validate-event-cfgs event-cfgs)
