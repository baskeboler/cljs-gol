(ns cljs-gol.renderer
    (:require [thi.ng.color.core :as colors]
              [thi.ng.math.core :as math]
              [thi.ng.geom.core :as geo]
              [thi.ng.geom.vector :as vectors])
    (:import [thi.ng.color.core.RGBA]))
(defprotocol Renderer
  (screen-size [this])
  (draw-rect [this x y w h color filled?] [this x y w h])
  (draw-text [this text ch lh cw x y bold?])
  (clear-screen [this color])
  (pipeline-flush [this])
  (pipeline-push [this operation])
  (last-render [this])
  (fps [this])
  (add-event-listener [this event-name handler-fn])
  (animation-push [this shape-seq])
  (animation-step [this]))

(defn calc-fps [renderer]
  (let [current-fps (:calculated-fps renderer 0)
        frames (:frames-rendered renderer)
        frame-time (- (js/Date.now) (:last-render-time renderer))
        new-fps (-> (/ frame-time 1000.0)
                    (+ current-fps))]))
(defn check-first-render [renderer]
  (if (not= 0 (:frames-rendered renderer))
    renderer
    (-> renderer
        (assoc :first-render-time (js/Date.now)))))

(defrecord CanvasRenderer
    [canvas ctx height width
     render-queue fps
     last-render-time frames-rendered
     animation-pipeline]
  Renderer
  (screen-size [this] [(:width this) (:height this)])
  (draw-rect [this x y w h ^RGBA color filled?]
     (let [ctx       (:ctx this)
           render-fn (fn []
                       (cond
                         filled? (do
                                   (set! (.-fillStyle ctx) @(colors/as-css color))
                                   (. ctx (fillRect x y w h)))
                         :else (do
                                 (set! (.-strokeStyle ctx) @(colors/as-css color))
                                 (. ctx (strokeRect x y w h)))))]
       (pipeline-push this render-fn)))
  (draw-rect [this x y w h] (draw-rect this x y w h colors/BLACK true))
  (draw-text [this text ch lh cw x y bold?]
    (println text)
    this)

  (pipeline-push [this operation] (-> this (update :render-queue conj operation)))
  (pipeline-flush [this]
    (doseq [op (->> (:render-queue this) 
                    (into []))]
      (op))
    (-> this
        (assoc :render-queue [])
        check-first-render
        (assoc :last-render-time (js/Date.now))
        (update :frames-rendered inc)
        animation-step))

  (clear-screen [this color]
    (draw-rect this 0 0 (:width this) (:height this) color true))
  (last-render [this] (:last-render-time this))
  (fps [this] (:fps this))
  (add-event-listener [this event-name handler-fn]
    (. (:canvas this) (addEventListener event-name handler-fn))
    this)
  (animation-push [this shape-seq]
    (-> this
        (update :animation-pipeline conj shape-seq)))
  (animation-step [{:keys [animation-pipeline] :as this}]
    (let [shapes (->> (mapv first animation-pipeline)
                      (filterv (comp not nil?)))
          new-pipeline (->> (mapv (comp vec rest) animation-pipeline)
                            (filterv (comp not empty?)))]
      (-> (reduce
           (fn [res shape]
             (draw shape res))
           this
           shapes)
          (assoc :animation-pipeline new-pipeline)))))

(defn new-renderer [element-id]
  (let [canvas-parent (. js/document (getElementById element-id))
        canvas (. js/document (createElement "canvas"))
        ctx (. canvas (getContext "2d"))
        w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    (set! (.. ctx -canvas -width) w)
    (set! (.. ctx -canvas -height) h)
    (set! (. ctx -strokeStyle) colors/WHITE)
    (. canvas-parent (appendChild canvas))
    (map->CanvasRenderer
     {:canvas canvas
      :ctx ctx
      :width w
      :height h
      :fps 90
      :render-queue []
      :last-render-time 0
      :frames-rendered 0})))

(defn render-loop [renderer-atom update-fn render-fn]
  (println "setting up render loop")
  (fn this-func []
    (js/requestAnimationFrame this-func)
    (let [fps-value (fps @renderer-atom)
          fps-interval (/  1000.0 fps-value)
          last-render-ts (last-render @renderer-atom)
          elapsed (- (js/Date.now) last-render-ts)]
      (when (> elapsed fps-interval)
        (render-fn))
      (update-fn))))


(defprotocol PShape
  (draw [this ^Renderer renderer] "returns the renderer")
  (translate [this dx dy] "returns new shape")
  (scale [this sx sy] [this q] "returns new shape")
  (center [this]) "returns a vec2")

(defrecord Rectangle [x y w h color filled?]
  PShape
  (draw
    [{:keys [x y w h color filled?] :as this}
     ^Renderer renderer]
    (draw-rect renderer x y w h color filled?))
  (translate [this x y]
    (-> this
        (update :x + x)
        (update :y + y)))
  (center [{:keys [x y w h] :as this}]
    (let [upper-left (vectors/vec2 x y)
          lower-right (math/+ upper-left (vectors/vec2 w h))]
      (math/mix upper-left lower-right 0.5)))
  (scale [this sx sy]
    (-> this
        (update :x * sx)
        (update :y * sy)
        (update :w * sx)
        (update :h * sy)))
  (scale [this q]
    (scale this q q)))

(defn normalize-color [c]
  (colors/css c))

(defn make-rect
  [{:keys [x y w h color filled?]
    :or {color colors/BLACK filled? true}
    :as spec}]
  (map->Rectangle (merge spec {:color color :filled? filled?})))

(defn rect-sequence
  ([rect frames src-color target-color x-offset y-offset]
   (->>
    (for [q (range 0 1.1 (/ 1 frames))]
      (-> rect (assoc :color (math/mix src-color target-color q))))
    (into [])))
  ([rect frames src-color target-color]
   (rect-sequence rect frames src-color target-color 0 0)))
