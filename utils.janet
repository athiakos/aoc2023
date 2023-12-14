

(defn heap [n]
  (array/new n))
  

(defn heap/push [heap v]
  (array/push heap v)

  (defn recur [&opt child]
    (default child (dec (length heap)))
    (let [parent (math/floor (/ (dec child) 2))]
      (when (< ((get heap child) :prio)
		((get heap parent :prio)))
	(let [tmp (get heap child)]
	  (set heap child (get heap parent))
	  (set parent tmp)
	  (recur parent))))
    (recur)
    heap))

(defn heap/pop [heap])
