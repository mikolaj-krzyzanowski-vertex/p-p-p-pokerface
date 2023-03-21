(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (let [rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14 }]
        (rank-map fst)))))


(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn item-number [item-type number hand]
  (let [items (map item-type hand)]
    (let [item-freq (vals (frequencies items))]
      (contains? (set item-freq) number))))

(defn pair? [hand]
  (item-number rank 2 hand))

(defn three-of-a-kind? [hand]
  (item-number rank 3 hand))

(defn four-of-a-kind? [hand]
  (item-number rank 4 hand))

(defn flush? [hand]
  (item-number suit 5 hand))

(defn full-house? [hand]
  (and (item-number rank 2 hand) (item-number rank 3 hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (let [rank-freq (vals (frequencies ranks))]
      (let [number-of-pairs (count (filter (fn [x] (= x 2)) rank-freq))]
        (or (= number-of-pairs 2) (four-of-a-kind? hand))))))



(defn straight? [hand]
  (let [ranks-sorted (sort (map rank hand)) ]
    (let [first-rank (first ranks-sorted)
          last-rank (last ranks-sorted)]
      (if (and (= first-rank 2) (= last-rank 14))
        (let [ranks-reduced (drop-last ranks-sorted)]
          (= ranks-reduced (range 2 6)))
        (= ranks-sorted (range (first ranks-sorted) (+ (last ranks-sorted) 1)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (let [hand-results (filter (fn [x] ((first x) hand)) checkers)]
      (let [values (map second hand-results)]
        (apply max values)))))
