(import spork/misc :as utils)

(def example (slurp "day5.example.txt"))
(def input (slurp "day5.txt"))


(def grammar
  ~{:seeds (group (* "seeds:" (constant "seeds") (group (some (* :s (number :d+)))) :newline))
    :newline "\n"
    :number-line (group (* (some (* (? :s) (number :d+))) :newline))
    :map-name (* (<- (* (some :a) "-to-" (some :a))) " map:" :newline)
    :map (group (* :map-name :matrix))
    :matrix (/ (some :number-line) ,(fn [line] (partition 3 line)))
    :main (* :seeds :newline
	     (repeat 7 (* :map (? :newline))))})

(defn from-ranges [ranges x]
  (let [result (prompt :exit
		  (loop [[dst src len] :in ranges
			 :when (<= src x (+ src (dec len)))]
		    (return :exit (+ dst (- x src)))))]
    (case result
      nil x
      result)))

(defn input->table [input]
  (tabseq [[name values] :in (array/slice input 1)
	   :let [key (keyword name)]]
	  key (fn [x] (from-ranges values x))))

(defn seed->location (table seed)
  (->> seed
       ((table :seed-to-soil))
       ((table :soil-to-fertilizer))
       ((table :fertilizer-to-water))
       ((table :water-to-light))
       ((table :light-to-temperature))
       ((table :temperature-to-humidity))
       ((table :humidity-to-location))))

(defn part1 [input]
  (let [seeds (utils/gett input 0 1)
	table (input->table input)]
    (min ;(seq [seed :in seeds]
	       (seed->location table seed)))))

(defn part2 [input]
  (var lowest nil)
  (let [seeds (partition 2 (utils/gett input 0 1))
	table (input->table input)]
    (loop [[start len] :in seeds
	   x :range [start (+ start len)]
	   :let [location (seed->location table x)]
	   :when (< location lowest)]
      (set lowest location))
    lowest))


