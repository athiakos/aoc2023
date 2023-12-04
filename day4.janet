(import spork/misc :as utils)

(def example (slurp "day4.example.txt"))
(def input (slurp "day4.txt"))

(def grammar
  '{:card (* "Card" :ws (number :d+) ":")
    :numbers (* (number :d+) (+ :ws :newline))
    :winning (group (some :numbers))
    :selection (group (some :numbers))
    :newline "\n"
    :sep "|"
    :ws (some " ")
    :line (group (* :card :ws :winning :sep :ws :selection))
    :main (some :line)})

(defn winning-numbers [game]
  (seq [number :in (utils/second game)
	:when (find |(= $ number) (utils/third game))]
       number))

(defn winnings [games]
  (tabseq [game :in games
	   :let [i (first game)]]
	  i (length (winning-numbers game))))

(defn score [game]
  (let [items (length game)]
    (case items
      0 0
      1 1
      (math/pow 2 (dec items)))))


(defn part1 [input]
  (->> (peg/match grammar input)
       (filter truthy?)
       (map winning-numbers)
       (map score)
       (sum)))


(defn part2 [input]
  (var i 0)
  (var game 1)
  (var counts (tabseq [[id,_,_] :in input] id 1))

  (let [scores (winnings input)]
    (while (< i (get counts game 0))
      (when-let [x (scores game)
		 _ (> x 0)
		 start (inc game)
		 end (+ start x)]
	(for k start end
	  (set (counts k) (inc (counts k)))))
      (+= i 1)
      (when (= i (counts game))
	(set i 0)
	(+= game 1))))
  (->> (values counts) (sum)))

