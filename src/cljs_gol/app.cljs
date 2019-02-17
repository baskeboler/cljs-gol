(ns cljs-gol.app
  (:require [reagent.core :as r :refer [atom]]
            [cljs-gol.renderer :as renderer]))

(defonce the-universe (r/atom nil))
(defonce the-renderer (r/atom nil))

(def canvas-style {:position :absolute
                   :display :block
                   :top "50%"
                   :left "50%"
                   :transform "translate(-50%, -50%)"})

(defprotocol Cells
  (tick [this]))

(defrecord Universe [width height cells rendered?])

(defn cell-index [u x y] (+ (* y (:width u)) x))
(defn get-cell [u x y]
  (when (and (< x (:width u))
             (<= 0 x)
             (< y (:height u))
             (<= 0 y))
    (let [idx (cell-index u x y)]
      (nth (:cells u) idx))))

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
(defn make-universe [w h]
  (let [cells (for [_ (range w)
                    _ (range h)]
                (if (> (rand) 0.7) :alive :dead))]
    (map->Universe
     {:width w
      :height h
      :cells (vec cells)
      :rendered? false})))


(defn canvas-universe-update []
  (when (-> @the-universe :rendered?)
    (println "updating")
    (let [new-u (-> (tick @the-universe)
                    (assoc :rendered? false))
          cell-w (/ (:width @the-renderer) (:width @the-universe))
          cell-h (/ (:height @the-renderer) (:height @the-universe))
          cells (for [i (range (:width new-u))
                      j (range (:height new-u))
                      :let [state (get-cell new-u i j)
                            color (if (= :alive state) "black" "white")
                            x (* cell-w i)
                            y (* cell-h j)]]
                     {:x x :y y :color color})
          new-renderer (reduce (fn [res op]
                                 (renderer/draw-rect res (:x op) (:y op) cell-w cell-h (:color op) true))
                               @the-renderer
                               cells)]
      (reset! the-universe new-u)
      (reset! the-renderer new-renderer))))    

(defn canvas-universe-render []
  (when-not  (-> @the-universe :rendered?)
    (println "rendering")
    (reset! the-universe (-> @the-universe (assoc :rendered? true)))
    (reset! the-renderer (renderer/pipeline-flush @the-renderer))))


(defn mount-components! []
  (reset! the-universe (make-universe 100 50))
  (reset! the-renderer (renderer/new-renderer "root"))
  (let [the-loop  (renderer/render-loop
                   the-renderer
                   canvas-universe-update
                   canvas-universe-render)]
    (the-loop)))

(defn init! []
  (println "hello world!")
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
