(ns cljs-gol.app
  (:require [reagent.core :as r :refer [atom]]
            [cljs-gol.renderer :as renderer]))

(defonce the-universe (r/atom nil))
(defonce the-renderer (r/atom nil))

(defprotocol Cells
  (tick [this]))

(defprotocol InsertCells
  (insert [this other x y]))

(defrecord Universe [width height cells rendered?])

(defn cell-index [u x y] (+ (* y (:width u)) x))
(defn get-cell [u x y]
  (when (and (< x (:width u))
             (<= 0 x)
             (< y (:height u))
             (<= 0 y))
    (let [idx (cell-index u x y)]
      (nth (:cells u) idx))))

(defn set-cell-state [u x y state]
  (if (and (< x (:width u))
           (<= 0 x)
           (< y (:height u))
           (<= 0 y))
    (let [idx (cell-index u x y)]
      (-> u
          (update :cells assoc idx state)))
    u))

(defn neighbors [u x y]
  (let [n        (-> []
                     (conj (get-cell u (inc x) y))
                     (conj (get-cell u (dec x) y))
                     (conj (get-cell u x (inc y)))
                     (conj (get-cell u x (dec y)))
                     (conj (get-cell u (inc x) (inc y)))
                     (conj (get-cell u (dec x) (dec y)))
                     (conj (get-cell u (dec x) (inc y)))
                     (conj (get-cell u (inc x) (dec y))))]
    (filterv (comp not nil?) n)))

(defn next-gen [u x y]
  (let [ns (neighbors u x y)
        a-count (reduce + 0 (mapv #(if (= % :alive) 1 0) ns))
        cell (get-cell u x y)]
    (cond
      (= cell :dead) (if (= 3 a-count) :alive :dead)
      (= cell :alive) (cond
                        (every? (partial = :dead) ns) :dead
                        (< a-count 2) :dead
                        (> a-count 3) :dead
                        :else :alive)
      :else :dead)))


(extend-protocol Cells
  Universe
  (tick [this]
    (-> this
        (assoc :cells
               (into []
                     (for [j (range (:height this))
                           i (range (:width this))]
                       (next-gen this i j)))))))

(extend-protocol InsertCells
  Universe
  (insert [this other x y]
    (let [other-w (:width other)
          other-h (:height other)
          inserts (for [j (range other-h)
                        i (range other-w)]
                    {:x (+ x i) :y (+ y j) :state (get-cell other i j)})]
      (reduce (fn [res op]
                (set-cell-state res (:x op) (:y op) (:state op)))
              this
              inserts))))

(defn make-universe [w h]
  (let [cells (for [_ (range w)
                    _ (range h)]
                (if (> (rand) 0.4) :alive :dead))]
    (map->Universe
     {:width w
      :height h
      :cells (vec cells)
      :rendered? false})))

(defn make-creature [w h cells]
  (map->Universe
   {:width w
    :height h
    :cells (mapv #(if (= 0 %) :dead :alive) cells)
    :rendered? false}))

(defn small-block []
  (map->Universe
   {:width 2
    :height 2
    :cells [:alive :alive
            :alive :alive]
    :rendered? false}))

(defn glider []
  (make-creature
   3 3
   [1 0 1
    0 1 1
    0 1 0]))

(defn grower []
  (make-creature
   5 5
   [1 1 1 0 1
    1 0 0 0 0
    0 0 0 1 1
    0 1 1 0 1
    1 0 1 0 1]))


(defn click-handler [evt]
  (let [x (. evt -x)
        y (. evt -y)
        cell-w (/ (:width @the-renderer) (:width @the-universe))
        cell-h (/ (:height @the-renderer) (:height @the-universe))
        u-x (long (/ x cell-w))
        u-y (long (/ y cell-h))]
    (println "adding block to " u-x ", " u-y)
    (swap! the-universe insert (grower) u-x u-y)
    (swap! the-renderer assoc :dirty? true)))

(defn canvas-universe-update []
  (when (-> @the-universe :rendered?)
    (let [new-u (-> (tick @the-universe)
                    (assoc :rendered? false))
          dirty? (:dirty? @the-renderer false)
          cell-w (/ (:width @the-renderer) (:width @the-universe))
          cell-h (/ (:height @the-renderer) (:height @the-universe))
          cells (for [i (range (:width new-u))
                      j (range (:height new-u))
                      :let [old-state (get-cell @the-universe i j)
                            state (get-cell new-u i j)
                            color (if (= :alive state) "black" "white")
                            x (* cell-w i)
                            y (* cell-h j)]
                      :when (or
                             dirty?
                             (not= state old-state))]
                     {:x x :y y :color color})
          new-renderer (reduce (fn [res op]
                                 (renderer/draw-rect res (:x op) (:y op) cell-w cell-h (:color op) true))
                               @the-renderer
                               cells)]
      (reset! the-universe new-u)
      (reset! the-renderer (-> new-renderer (assoc :dirty? false))))))

(defn canvas-universe-render []
  (when-not  (-> @the-universe :rendered?)
    (reset! the-universe (-> @the-universe (assoc :rendered? true)))
    (reset! the-renderer (renderer/pipeline-flush @the-renderer))))


(defn mount-components! []
  (reset! the-universe (make-universe 100 50))
  (reset! the-renderer (renderer/new-renderer "root"))
  (swap! the-renderer renderer/add-event-listener  "click" click-handler)
  (. js/window
     (addEventListener
      "resize"
      (fn [evt]
        (let [w2 (.-innerWidth js/window)
              h2 (.-innerHeight js/window)]
          (println "resizing!")
          (set! (.. (:ctx @the-renderer) -canvas -width) w2)
          (set! (.. (:ctx @the-renderer) -canvas -height) h2)
          (swap! the-renderer assoc :dirty? true)))))

  (let [the-loop  (renderer/render-loop
                   the-renderer
                   canvas-universe-update
                   canvas-universe-render)]
    (the-loop)))

(defn init! []
  (println "init!")
  (mount-components!))

(defn stripes []
  (let [w 50
        h 50
        cells (->>
               (for [i (range w) j (range h)]
                 (if (= 0 (mod i 2)) :alive :dead))
               (into []))]
    (map->Universe {:width w
                    :height h
                    :cells cells
                    :rendered? true})))
