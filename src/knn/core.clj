(ns knn.core
  (:gen-class))

(defn euclidean-distance
  [vec1 vec2]
  (Math/sqrt
    (reduce + (map #(Math/pow (- %1 %2) 2) vec1 vec2))))

(defn nearest-neighbors
  [samples query k]
  (take k
    (sort-by :score
      (map
        #(assoc % :score (euclidean-distance query (:pos %)))
        samples))))

(defn knn
  [samples query k]
  (let [votes (nearest-neighbors samples query k)
        vote-freq (frequencies (map :class votes))]
        (key (apply max-key val vote-freq))))

(def training-set
  [{:pos [ 2  0] :class "OSX"}
   {:pos [ 1  3] :class "Windows"}
   {:pos [-1  0] :class "Unix"}
   {:pos [-9  1] :class "Unix"}
   {:pos [ 8 -8] :class "OSX"}
   {:pos [ 4  1] :class "OSX"}
   {:pos [ 0  0] :class "OSX"}
   {:pos [ 4  2] :class "Unix"}
   {:pos [ 2 -3] :class "OSX"}
   {:pos [ 8 -3] :class "OSX"}])

(defn -main
 [& args]
 (let [query [4 2]
       k 4]
   (println query "-" (knn training-set query1 k))))
