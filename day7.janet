(def example (slurp "day7.example.txt"))
(def input (slurp "day7.txt"))


(defn int-or-symbol [str]
  (let [x (scan-number str)]
    (case (type x)
      :number x
      :nil (symbol str))))

(defn parse-hand [hand]
  (->> hand
       (string/bytes)
       (map string/from-bytes)
       (map int-or-symbol)
       (freeze)))

(def grammar
  ~{:hand (/ (<- (repeat 5 :w)) ,parse-hand)
    :bid (number :d+)
    :newline "\n"
    :line (group (* :hand :s :bid :newline))
    :main (some :line)})


(defn best-card [hand]
  (let [freq (filter |(not (= (get $ 0) 'J)) (pairs (frequencies hand)))
	m (max ;(map |(get $ 1) freq))]
    (->> freq
	 (filter |(= (get $ 1) m))
	 (map first)
	 (sorted-by order-joker-card)
	 (last))))

(defn promote [hand]
  (if (= hand '( J J J J J))
    '(A A A A A)
      (do
	(let [best (best-card hand)
	      idx (find-index |(= $ 'J) hand)]
	  (array/concat
	   (array/slice hand 0 idx)
	   [best]
	   (array/slice hand (inc idx))))	))
  )

(defn score-hand [hand]
  (match (sorted (values (frequencies hand)) >)
    [1 1 1 1 1] :high-card
    [2 1 1 1] :one-pair
    [2 2 1] :two-pair
    [3 1 1] :three-of-kind
    [3 2] :full-house
    [4 1] :four-of-kind
    [5] :five-of-kind
    _ (error :unknown)))

(defn score-joker-hand [hand]
  (if (any? (map |(= $ 'J) hand))
    (score-joker-hand (promote hand))
    (score-hand hand)))


(defn score [joker? hand]
  (if joker?
    (score-joker-hand hand)
    (score-hand hand)))


(defn order [joker? card]
  (if joker?
    (order-joker-card card)
    (order-card card)))

(defn order-card [card]
  (let [ord {2 1 3 2 4 3 5 4 6 5 7 6 8 7 9 8 'T 9 'J 10 'Q 11 'K 12 'A 13}]
    (get ord card -1)))

(defn order-joker-card [card]
  (let [ord {'J 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 'T 10 'Q 11 'K 12 'A 13}]
    (get ord card -1)))


(defn order-hand [hand &opt joker?]
  (default joker? false)
  (let [ord {:high-card 1
	     :one-pair 2
	     :two-pair 3
	     :three-of-kind 4
	     :full-house 5
	     :four-of-kind 6
	     :five-of-kind 7}]
    (->> hand
	 (score joker?)
	 (ord))))


(defn cmp-equal-hands [joker? a b]
  (prompt :exit
     (loop [[x y] :in (partition 2 (interleave a b))
	    :let [score-a (order joker? x)
		  score-b (order joker? y)]]
       #(print (string/format "%m:%d %m:%d" x score-a y score-b))
       (cond
	 (< score-a score-b) (return :exit true)
	 (> score-a score-b) (return :exit false)))))


(defn cmp-hands [joker? x y]
  (let [a (get x 0)
	b (get y 0)
	score-a (order-hand a joker?)
	score-b (order-hand b joker?)]
    (cond
      (< score-a score-b) true
      (> score-a score-b) false
      (= score-a score-b) (cmp-equal-hands joker? a b))))


(defn rank-hands [hands joker?]
  (let [comparator (fn [a b] (cmp-hands joker? a b))]
    (seq [[i x] :pairs (sorted hands comparator)]
       (tuple (inc i) (get x 1)))))


(defn part1 [hands]
  (->> (rank-hands hands false)
       (map |(* (get $ 0) (get $ 1)))
       (sum)))


(defn part2 [hands]
  (->> (rank-hands hands true)
       (map |(* (get $ 0) (get $ 1)))
       (sum)))
