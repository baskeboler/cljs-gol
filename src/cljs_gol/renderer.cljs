(ns cljs-gol.renderer)

(defprotocol Renderer
  (screen-size [this])
  (draw-rect [this x y w h color filled?] [this x y w h])
  (draw-text [this text ch lh cw x y bold?])
  (clear-screen [this color])
  (pipeline-flush [this])
  (pipeline-push [this operation])
  (last-render [this])
  (fps [this])
  (add-event-listener [this event-name handler-fn]))

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

(defrecord CanvasRenderer [canvas ctx height width render-queue fps last-render-time frames-rendered]
  Renderer
  (screen-size [this] [(:width this) (:height this)])
  (draw-rect [this x y w h color filled?]
     (let [ctx       (:ctx this)
           render-fn (fn []
                       (cond
                         filled? (do
                                   (set! (.-fillStyle ctx) color)
                                   (. ctx (fillRect x y w h)))
                         :else (do
                                 (set! (.-strokeStyle ctx) color)
                                 (. ctx (strokeRect x y w h)))))]
       (pipeline-push this render-fn)))
  (draw-rect [this x y w h] (draw-rect this x y w h "black" true))
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
        (update :frames-rendered inc)))

  (clear-screen [this color]
    (draw-rect this 0 0 (:width this) (:height this) color true))
  (last-render [this] (:last-render-time this))
  (fps [this] (:fps this))
  (add-event-listener [this event-name handler-fn]
    (. (:canvas this) (addEventListener event-name handler-fn))
    this))

(defn new-renderer [element-id]
  (let [canvas-parent (. js/document (getElementById element-id))
        canvas (. js/document (createElement "canvas"))
        ctx (. canvas (getContext "2d"))
        w (.-innerWidth js/window)
        h (.-innerHeight js/window)]
    (set! (.. ctx -canvas -width) w)
    (set! (.. ctx -canvas -height) h)
    (set! (. ctx -strokeStyle) "white")
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

