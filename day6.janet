(import spork/misc :as utils)

(def example (slurp "day6.example.txt"))
(def input (slurp "day6.txt"))

(defn string->int [arr]
  
  (->> arr
       (string/join)
       (utils/string->int)))

(def grammar
  ~{:column (* (some :s) (number :d+))
    :line (group (some :column))
    :time (* "Time:" :line :newline)
    :distance (* "Distance:" :line :newline)
    :newline "\n"
    :input (/ (* :time :distance) ,format-input)
    :main :input})

(def grammar2
  ~{:column (* (some :s) (<- :d+))
    :line (/ (group (some :column)) ,string->int)
    :time (* "Time:" :line :newline)
    :distance (* "Distance:" :line :newline)
    :newline "\n"
    :main (* :time :distance)})

(defn format-input [time distance]
  (partition 2 (interleave time distance)))

(defn winning-setup [race]
  (let [[time distance] race
	lowest (math/ceil (/ distance time))]
    (seq [u :down-to [(dec time) lowest]
	  :let [t (- time u)
		d (* t u)]
	  :when (> d distance)]
	 u)))


(defn part1 [input]
  (->> (get (peg/match grammar input) 0)
       (map winning-setup)
       (map length)
       (apply *)))

(defn part2 [input]
  (->> (peg/match grammar2 input)
       (winning-setup)
       (length)))