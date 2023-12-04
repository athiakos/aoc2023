
(def example (slurp "day2.example.txt"))

(def grammar
  '{:game (* "Game " (number :d+) ":")
    :item (group (* :s (number :d+) :s (<- :color) (? ",")))
    :subset (group (some :item))
    :set (* :subset (set "\n;"))
    :line (* :game (some :set))
    :color (+ "blue" "red" "green")
    :main (some (group :line))})


(defn game-possible? [game]
  (prompt :exit
     (let [sets (array/slice game 1)]
       (loop [set :in sets
	      [count color] :in set]
	 (case color
	   "red" (when (> count 12) (return :exit false))
	   "green" (when (> count 13) (return :exit false))
	   "blue" (when (> count 14) (return :exit false))))
       true)))

(defn fewest [game]
  (let [colors @{"red" 0 "green" 0 "blue" 0}
	sets (array/slice game 1)]
    (loop [set :in sets
	   [count color] :in set]
      (set (colors color) (max count (get colors color))))
    colors))
	     

(defn part1 [input]
  (->> (peg/match grammar input)
       (filter game-possible?)
       (map first)
       (sum)))
  

(defn part2 [input]
  (->> (peg/match grammar input)
       (map fewest)
       (map |(* ;(values $)))
       (sum)))


(let [input (slurp "day2.txt")]
  (pp (part1 input))
  (pp (part2 input)))