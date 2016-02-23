(ns tictacky.core
  (:require [clojure.pprint :as pprint]))

;; Checks board state and returns a winner if any, else returns nil
(defn winner [board]
  (let [candidates (concat board
                           (apply map list board)
                           [(map nth board [0 1 2]) (map nth board [2 1 0])])]
    (first (some #{[:x :x :x] [:o :o :o]} candidates))))

;; Returns opponent symbol given the player's one
(defn opp [p] (if (= p :x) :o :x))

(defn node-max [& nodes]
  (apply max-key :cost nodes))

(defn node-min [& nodes]
  (apply min-key :cost nodes))

;; Finds empty cells in board
(defn empty-cells [board]
  (apply concat
         (map #(keep-indexed (fn [index val] (if (= val :e) [%2 index] nil)) %)
            board (iterate inc 0))))

(defn generate-game-tree [board turn-player current-player position]
  (let [new-board  (update-in board position (fn [old] current-player))
        fill-cells (empty-cells new-board)
        w          (winner new-board)
        next-node  {:pos position :board new-board :player current-player :nodes nil}
        opponent   (opp current-player)
        f          #(generate-game-tree new-board turn-player opponent %)]
    (cond
      (= w turn-player)       (assoc next-node :cost 1)
      (= w (opp turn-player)) (assoc next-node :cost -1)
      (empty? fill-cells)     (assoc next-node :cost 0)
      :else                   (assoc next-node :nodes (map f fill-cells)))))

;; Generate game tree based on current board state and player to play
(defn game-tree [board player]
  ;; Check terminal conditions
  (let [emptycells (empty-cells board)]
    (if (or (empty? emptycells) (winner board))
      nil
      {:pos   nil
       :board board
       :nodes (map #(generate-game-tree board player player %) emptycells)})))

;; Finds the max/min cost route for player
;; This impl compares the cost of the leaf values at the root
(defn minimax [board turn-player current-player node]
  (let [next-nodes   (:nodes node)
        opponent     (opp current-player)
        same-player? (= turn-player current-player)
        fn            #(minimax board turn-player opponent %)]
    (cond
      (nil? next-nodes) node
      same-player?      (let [max-node (apply node-max (flatten (map fn next-nodes)))]
                          (-> node
                             (assoc :cost (:cost max-node))
                             (assoc :best (:pos max-node))))
      :else             (let [min-node (apply node-min (flatten (map fn next-nodes)))]
                          (-> node
                             (assoc :cost (:cost min-node))
                             (assoc :best (:pos min-node)))))))

;; Finds the best move for a player based on minimax algorithm
(defn best-move [board player]
  (let [gtree          (game-tree board player)
        new-tree       (minimax board player player gtree)]
    (:best new-tree)))

(def board [[:e :o :x]
            [:x :e :x]
            [:o :x :o]])

 (defn -main
  [& args]
   (let [board [[:e :o :x]
                [:x :e :x]
                [:o :x :o]]]
     (println (str "Best move location [y x] => " (best-move board :o)))))
