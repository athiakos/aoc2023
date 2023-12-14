(def example (slurp "day8.example.txt"))
(def example2 (slurp "day8.example2.txt"))

(defn parse-line [node left right]
  {:value node :left left :right right})

(def grammar
  ~{:dir (+ (/ "R" ,:right) (/ "L" ,:left))
    :path  (group (* (some :dir) :newline :newline))
    :node (<- (repeat 3 :w))
    :newline "\n"
    :line (/ (* :node :s "=" :s "(" :node "," :s :node ")" :newline) ,parse-line)
    :main (* :path (some :line))})


(defn make-tree [input]
  (var tree @{})
  (tabseq [node :in (array/slice input 1)
	   :let [key (node :value)]]
	  key {:left (node :left) :right (node :right)}))


(defn walk-tree [tree path]
  (defn recur [start &opt count]
    (default count 0)
    (var node (thaw start))
    (var i count)
    (prompt :top
       (each dir path
	 (set node ((tree (freeze node)) dir))
	 (set i (inc i))
	 (when (= node @"ZZZ")
	   (return :top)))
       (if (= node "ZZZ")
	 i
	 (recur node i))))
  (recur "AAA"))


(defn walk [tree path]
  (var count 0)
  
  (defn recur [nodes rest]
    (cond
      (->> nodes (map |(= "Z" (string/slice $ -2))) (all truthy?)) count
      (empty? rest) (recur nodes path)
      (let [dir (get rest 0)
	    nodes (seq [node :in nodes] ((tree node) dir))]
	(set count (inc count))
	(recur nodes (array/slice rest 1)))))

  (recur (filter |(= "A" (string/slice $ -2)) (keys tree)) path))


(defn part1 [input]
  (let [input (peg/match grammar input)
	tree (make-tree input)
	path (get input 0)]
    (walk-tree tree path)))

(defn part2 [input]
  (let [input (peg/match grammar input)
	tree (make-tree input)
	path (get input 0)]
    (walk tree path)))