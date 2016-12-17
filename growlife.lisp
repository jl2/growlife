;;;; growlife.lisp

(in-package #:growlife)


(declaim (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 0)))

(defstruct transition
  (low 0.3)
  (high 0.75)
  (death -0.3)
  (life 0.05))

(defun trans (low high death life)
  (make-transition :low low :high high
                   :death death :life life))
  

(defun new-board (width height &key (num 1000))
  (let ((board (make-array `(,width ,height) :element-type 'real :initial-element 0.0)))
    (dotimes (i num)
      (let ((i (random (array-dimension board 0)))
            (j (random (array-dimension board 1))))
        (setf (aref board i j) (random 1.0))))
    (list board)))
  
(defun count-neighbors (grid i j)
  "Count the neighbors of grid location i,j"
  (let ((ip (if (> 0 (- i 1)) (- (array-dimension grid 0) 1) (- i 1)))
        (jp (if (> 0 (- j 1)) (- (array-dimension grid 1) 1) (- j 1)))
        (in (if (>= i (- (array-dimension grid 0) 1)) 0 (+ i 1)))
        (jn (if (>= j (- (array-dimension grid 1) 1)) 0 (+ j 1)))
        (count (aref grid i j)))
    (incf count (aref grid ip jp))
    (incf count (aref grid ip j))
    (incf count (aref grid ip jn))
    (incf count (aref grid i jp))
    (incf count (aref grid i jn))
    (incf count (aref grid in jp))
    (incf count (aref grid in j))
    (incf count (aref grid in jn))
    count))


(defun update-board (boards &key (transition (make-transition)))
  "Update old-grid based on neighbor counts, placing the results in new-grid."
  (if (< (length boards) 150)
      (let* ((old-grid (car boards))
             (new-grid (make-array (list (array-dimension old-grid 0) (array-dimension old-grid 1)) :element-type 'real :initial-element 0.0))
             (low-life (transition-low transition))
             (high-life (transition-high transition))
             (death-amount (transition-death transition))
             (life-amount (transition-life transition)))
        (loop for i from 0 below (array-dimension old-grid 0)
           do
             (loop for j from 0 below (array-dimension old-grid 1)
                do
                  (let ((nw (count-neighbors old-grid i j)))
                    (if (< low-life nw high-life)
                        (setf (aref new-grid i j) (clamp (+ (aref old-grid i j) life-amount) 0.0 1.0))
                        (setf (aref new-grid i j) (clamp (+ (aref old-grid i j) death-amount) 0.0 1.0))))))
        (cons new-grid boards))
      boards))


(defun draw-cube (verts faces)
  (labels ((set-normal (n)
             (gl:normal (aref n 0) (aref n 1) (aref n 2)))
           (set-vertex (index)
             (let ((v (aref verts index)))
               (gl:vertex (aref v 0) (aref v 1) (aref v 2))))
           (draw-face (vertex-indices normal)
             (set-normal normal)
             (gl:begin :quads)
             (map 'nil #'set-vertex vertex-indices)
             (gl:end)))
 
    (map 'nil #'(lambda (x) (draw-face (first x) (second x))) faces)))

(defun cube-at (cx cy cz dx dy dz)
  (let ((cube-verts #(#(0 0 0)
                      #(0 1 0)
                      #(1 1 0)
                      #(1 0 0)
                      #(0 0 1)
                      #(0 1 1)
                      #(1 1 1)
                      #(1 0 1)))
 
        (cube-norms '((#(4 7 6 5) #(0 0 1))
                      (#(5 6 2 1) #(0 1 0))
                      (#(1 2 3 0) #(0 0 -1))
                      (#(0 3 7 4) #(0 -1 0))
                      (#(4 5 1 0) #(-1 0 0))
                      (#(3 2 6 7) #(1 0 0)))))
    (gl:push-matrix)
    (gl:translate cx cy cz)
    (gl:scale dx dy dz)
    (draw-cube cube-verts cube-norms)
    (gl:pop-matrix)))

(defun draw-board (boards win-width win-height &key (multiplier 1.5))
  "Used OpenGL to display the grid."
  ;; (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:scale 0.5 0.5 0.5)
  (let* ((dx (/ win-width (array-dimension (car boards) 0)))
        (dz (/ win-height (array-dimension (car boards) 1)))
        (dy dz)
        (cx 0)
        (cy 0)
        (cz 0))
    ;; (gl:begin :quads)n
    (dolist (grid (reverse boards))
      (setf cx 0)
      (loop for i from 0 below (array-dimension grid 0)
         do
           (setf cz 0)
           (loop for j from 0 below (array-dimension grid 1)
              do
                (let ((perc (aref grid i j)))
                  (if (< 0.1 perc 0.7)
                      (progn
                        (gl:material :front :diffuse `#(0.1
                                                        ,(clamp (* multiplier perc) 0.0 1.0)
                                                        1.0
                                                        0.1
                                                        ,(clamp (* multiplier perc) 0.0 1.0)))
                        (cube-at cx cy cz dx dy dz))))
                (incf cz dz))
           (incf cx dx))
      (incf cy dy))
    (gl:pop-matrix)))

(defun start-life (&key
                     (board-width 50) (board-height 50) (board-depth 50)
                     (win-width 800) (win-height 800)
                     (num (truncate (* board-width board-height board-depth 0.25)))
                     (multiplier 1.5)
                     (fps 30)
                     (delay 20)
                     (fullscreen nil)
                     (transition (make-transition)))
  "Run the game of life in an SDL window."
  (let
      ((theta 0.0)
       (boards (new-board board-width board-height :num num))
       ;; boards is a cons cell pointing to two 2-d arrays of booleans
       (prev-tick 0) ;; prev-tick is the previous value of sdl-system-ticks when the board was updated
       (paused nil) ;; paused is t when paused, nil when not
       (trans (if (listp transition) (apply #'trans transition) transition))
       (xrot 0.0)
       (yrot 0.0)
       (zrot 0.0)
       (xoff 0.0)
       (yoff 0.0)
       (zoff 0.0)
       )
    (sdl:with-init
        ()
      ;; Setup the window and view
      (sdl:window win-width win-height
                  :fullscreen fullscreen
                  :opengl t
                  :opengl-attributes '((:sdl-gl-depth-size   24)
                                       (:sdl-gl-doublebuffer 1)))
      (setf (sdl:frame-rate) fps)
      
      (gl:viewport 0 0 win-width win-height)

      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 70 (/ win-height win-width) 1.0 50000.0)
      (glu:look-at 400 600 800 
                    50 50 50
                    0 1 0)
      ;; (glu:look-at 150.0 150.0 150.0 0.0 0.0 0.0 0.0 1.0 0.0)
      ;; (gl:ortho 0.0 win-width 0.0 win-height -1.0 1.0)

      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:clear-color 0 0 0 0)
      (gl:shade-model :smooth)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:draw-buffer :back)

      (gl:material :front :ambient-and-diffuse #(0.2 0.2 0.2 0.2))
      (gl:light :light0 :position #(60 60 60 0))
      (gl:light :light0 :diffuse #(1 1 1 1))
      (gl:light :light1 :position #(100 200 75 0))
      (gl:light :light1 :diffuse #(1 1 1 1))
      (gl:enable :cull-face :depth-test
                 :lighting :light0 :light1)

      (gl:clear :color-buffer :depth-buffer)
      
      ;; Draw the initial board
      (draw-board boards win-width win-height :multiplier multiplier)
      (sdl:update-display)

      ;; Handle events
      (sdl:with-events ()
        (:quit-event () t)

        (:mouse-motion-event
         (:state state :x x :y y :x-rel xrel :y-rel yrel)
         (if (= state 1)
             (progn
               (incf yrot (/ xrel 10))
               (incf xrot (/ yrel 10))
               ))
         (if (= state 4)
             (progn
               (incf xoff xrel)
               (incf yoff (- yrel))
               )))

        (:key-down-event
         ()
         ;; quit
         (when (or (sdl:key-down-p :sdl-key-q) (sdl:key-down-p :sdl-key-escape))
           (sdl:push-quit-event))

         ;; Reset to a random state
         (when (sdl:key-down-p :sdl-key-r)
           (setf boards (new-board board-width board-height :num num)))

         ;; Pause/unpause
         (when (sdl:key-down-p :sdl-key-p)
           (if paused
               (setf paused nil)
               (setf paused t))))

        (:video-expose-event () (sdl:update-display))

        (:idle
         ;; Check if it's time to update
         (if (> (- (sdl:system-ticks) prev-tick) delay)
             (progn
               (setf prev-tick (sdl:system-ticks))

               ;; Only update the board while not paused
               (when (not paused)
                 (setf boards (update-board boards :transition trans)))

               ;; Clear the screen and redraw
               (gl:clear :color-buffer :depth-buffer)
               (gl:push-matrix)
               (gl:translate xoff yoff zoff)
               (gl:rotate xrot 1.0 0.0 0.0)
               (gl:rotate yrot 0.0 1.0 0.0)
               (gl:rotate zrot 0.0 0.0 1.0)
               ;; (gl:translate (/ board-width -2.0) (/ board-height -2.0) (/ board-depth -2.0))
               (draw-board boards win-width win-height :multiplier multiplier)
               (gl:pop-matrix)
               (sdl:update-display))))))))
