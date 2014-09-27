(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
        (Integer/valueOf (str rnk))
        (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rnk))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (def ranks (map rank hand))
  (def values (vals (frequencies ranks)))
  (if (and (== (apply max values) 2)
           (== (count values)   4))
       true
       false))

(defn three-of-a-kind? [hand]
  (def ranks (map rank hand))
  (def values (vals (frequencies ranks)))
  (if (and (== (apply max values) 3) 
           (== (count values) 3))
       true
       false))

(defn four-of-a-kind? [hand]
  (def ranks (map rank hand))
  (def values (vals (frequencies ranks)))
  (if (and (== (apply max values) 4)
           (== (count values) 2))
   true
   false))

(defn flush? [hand]
  (def ranks   (map rank hand))
  (def suits  (map suit hand))
  (def smax  (apply max (vals (frequencies suits))))
  (def mi    (apply min ranks))
  (def mx   (apply max ranks))
  (if (and (== smax 5) 
       (not (= ranks (range mi (+ mx 1)))))
      true
      false))
      

(defn full-house? [hand]
  (def ranks (vec (map rank hand)))
  (def rvals (vals (frequencies ranks)))
  (if (and (== (apply max rvals) 3)
           (== (count rvals) 2))
      true
      false))
           

(defn two-pairs? [hand]
  (def ranks (map rank hand))
  (def values (vals (frequencies ranks)))
  (if (or 
         (and (== (apply max values) 2)
              (== (count values) 3))
         (four-of-a-kind? hand)
      ) 
      true
      false))


(defn straight? [hand]
  (def rt (map rank hand))
  (def vt (map suit hand))
  (def mix (apply min rt))
  (def mxx (apply max rt))
  (if (and (== mix 2) (== mxx 14)) (def rt (cons 1 (pop (vec rt)))) rt)
  (def srt (sort (vec rt)))
  (if (and (= srt (range (nth srt 0) (+ 1 (nth srt (- (count srt) 1)))))
           (>= (apply max (vals (frequencies vt))) 2))
      true
      false))


(defn straight-flush? [hand]
 (if (and (straight? hand)
          (flush?    hand))
     true
     false))

(defn high-card? [hand]
 (cond
   (pair? hand)            false
   (three-of-a-kind? hand) false
   (four-of-a-kind? hand)  false
   (full-house? hand)      false
   (two-pairs? hand)       false
   (straight? hand)        false
   (flush?    hand)        false
   (straight-flush? hand)  false
   :else                   true
  ))


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
   ))
