;;;; growlife.lisp

(in-package #:growlife)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 2) (size 0) (debug 3)))

(defparameter *fps* 60)

(defstruct transition
  (low 0.3)
  (high 0.65)
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
    (gl:pop-matrix)));; (defun cube-at (cx cy cz dx dy dz)
  ;; ;; (let ((cube-verts #(
  ;; ;;                     ;; Top corners
  ;; ;;                     #(-0.5 0.0 -0.5)
  ;; ;;                     #(-0.5 0.0 0.5 )
  ;; ;;                     #(0.5 0.0 0.5 )
  ;; ;;                     #(0.5 0.0 -0.5)
  ;; ;;                     ))

  ;; ;;       (cube-normals #(#(0.0 1.0 0.0)
                        
  ;; ;;                       ))
  ;; ;;       (indices #(
  ;; ;;                  #(0 1 2 3 0 0 0 0)
  ;; ;;                  )))

  ;; ;;   (gl:push-matrix)

  ;; ;;   (gl:translate cx cy cz)
  ;; ;;   (gl:scale dx dy dz)

  ;; ;;   (gl:with-primitives :quads
  ;; ;;     (loop for index across indices
  ;; ;;        do
  ;; ;;          (dotimes (i 4)
  ;; ;;            (gl:normal (aref (aref cube-normals (aref index (+ 4 i))) 0)
  ;; ;;                       (aref (aref cube-normals (aref index (+ 4 i))) 1)
  ;; ;;                       (aref (aref cube-normals (aref index (+ 4 i))) 2))
  ;; ;;            (gl:vertex (aref (aref cube-verts (aref index i)) 0)
  ;; ;;                       (aref (aref cube-verts (aref index i)) 1)
  ;; ;;                       (aref (aref cube-verts (aref index i)) 2)))))
  ;; ;;   (gl:pop-matrix))

  ;; )

(define-widget grow-life-animator (QGLWidget)
  ((theta :initform 0.0)
   (boards :initform (new-board 50 50))
   (paused :initform nil) ;; paused is t when paused, nil when not
   (trans :initform (make-transition))
   (xrot :initform 0.0)
   (yrot :initform 0.0)
   (zrot :initform 0.0)
   (zoom :initform 75.0))
  (:documentation "The scene-drawer widget draws a cube using the parameters specified in scene."))

(define-subwidget (grow-life-animator timer) (q+:make-qtimer grow-life-animator)
  (setf (q+:single-shot timer) nil))

(define-initializer (grow-life-animator setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background grow-life-animator) nil)
  (setf (q+:auto-buffer-swap grow-life-animator) nil))

(define-slot (grow-life-animator tick) ()
  (declare (connected timer (timeout)))
  (when (not paused)
    (setf boards (update-board boards :transition trans)))
  (q+:repaint grow-life-animator))

(define-override (grow-life-animator initialize-G-L) ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test)
  
  (gl:material :front :ambient-and-diffuse #(0.0 0.0 0.0 0.5))
  (gl:material :back :ambient-and-diffuse #(0.0 0.0 0.0 0.0))

  (gl:light :light0 :position #(-100 20 -100 0))
  (gl:light :light0 :diffuse #(1 1 1 1))

  (gl:light :light1 :position #(100 20 -100 0))
  (gl:light :light1 :diffuse #(1 1 1 1))

  (gl:light :light2 :position #(100 20 100 0))
  (gl:light :light2 :diffuse #(1 1 1 1))

  (gl:light :light3 :position #(-100 20 100 0))
  (gl:light :light3 :diffuse #(1 1 1 1))
  
  (gl:enable :cull-face :depth-test
             :lighting :light0 :light1 :light2 :light3))

(define-override (grow-life-animator resize-g-l) (width height)
  )

(define-override (grow-life-animator paint-g-l paint) ()
  "Handle paint events."
  
  (let* ((max-radius zoom)
         (width (q+:width grow-life-animator))
         (height (q+:height grow-life-animator))
         (x-aspect-ratio (if (< height width)
                             (/ height width 1.0d0)
                             1.0d0))
         (y-aspect-ratio (if (< height width)
                             1.0d0
                             (/ width height 1.0d0))))

    (with-finalizing 
        ;; Create a painter object to draw on
        ((painter (q+:make-qpainter grow-life-animator)))

      (q+:begin-native-painting painter)

      (gl:viewport 0 0 width height)

      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 70 (/ height width) 1.0 50000.0)
      (glu:look-at  zoom zoom zoom
                    0 0 0
                    0 1 0)

      (gl:clear-color 0 0 0 1)
      (gl:enable :line-smooth :polygon-smooth
                 :depth-test :depth-clamp :alpha-test)
      (gl:polygon-mode :front :line)
      (gl:polygon-mode :back :fill)

      (gl:matrix-mode :modelview)
      (gl:load-identity)

      (gl:clear :color-buffer :depth-buffer)

      (gl:push-matrix)
      
      (gl:rotate xrot 1 0 0)
      (gl:rotate yrot 0 1 0)
      (gl:rotate zrot 0 0 1)
      (gl:translate (/ (array-dimension (first boards) 0) -2.0) -50 (/ (array-dimension (first boards) 1) -2.0))
      (let ((dx 1.5)
            (dz 1.5)
            (dy 1.5)
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
                      (when (< 0.1 perc 0.7)
                        (gl:material :front :diffuse `#(0.0 0.0 ,(* (- perc 0.1) (/ 1.0 0.6))  1.0))
                        (gl:material :back :diffuse `#(0.0 0.0 0.0 1.0
                                                       ))
                        (cube-at cx cy cz dx dy dz)))
                    (incf cz dz))
               (incf cx dx))
          (incf cy dy)))
      (gl:pop-matrix)

      (q+:swap-buffers grow-life-animator)
      (q+:end-native-painting painter)
      ;; (trivial-garbage:gc)
      )))

(define-override (grow-life-animator key-press-event key-press) (ev)
  ;; (format t "Got key event ~a ~a ~a~%" (q+:key ev) ev (q+:modifiers ev))
  ;; (when (= (q+:modifiers ev) (enum-value (#_Qt::ShiftModifier)))
  ;;   (format t "Got shift modifier!~%"))
  (cond
    ((and (= (q+:key ev) (q+:qt.key_up)) (= (q+:modifiers ev) (enum-value (#_Qt::ShiftModifier))))
     (incf zoom -2))
    ((and (= (q+:key ev) (q+:qt.key_down)) (= (q+:modifiers ev) (enum-value (#_Qt::ShiftModifier))))
     (incf zoom 2))


    ((and (= (q+:key ev) (q+:qt.key_up)) (= (q+:modifiers ev) (enum-value (#_Qt::ControlModifier))))
     (incf zrot (/ pi -2)))
    ((and (= (q+:key ev) (q+:qt.key_down)) (= (q+:modifiers ev) (enum-value (#_Qt::ControlModifier))))
     (incf zrot (/ pi 2)))

    ((= (q+:key ev) (q+:qt.key_left))
     (incf yrot (/ pi -2)))

    ((= (q+:key ev) (q+:qt.key_right))
     (incf yrot (/ pi 2)))
    
    ((= (q+:key ev) (q+:qt.key_up))
     (incf xrot (/ pi 2)))

    ((= (q+:key ev) (q+:qt.key_down))
     (incf xrot (/ pi -2)))
    
    ((= (q+:key ev) (q+:qt.key_p))
     (setf paused (not paused)))

    ((= (q+:key ev) (q+:qt.key_x))
     (setf xrot 0))
    
    ((= (q+:key ev) (q+:qt.key_y))
     (setf yrot 0))

    ((= (q+:key ev) (q+:qt.key_z))
     (setf zrot 0))

    (t
     (call-next-qmethod))))

;; (define-override (grow-life-animator key-release-event) (ev)
;;   (format t "Got key event ~a ~a~%" (q+:key ev) ev)
;;   (cond ;; ((= (q+:key ev) (q+:qt.key_left))
;;         ;;  (decf angle-delta))
;;         ((= (q+:key ev) (q+:qt.key_p))
;;          (setf paused (not paused))
;;          )))

(define-override (grow-life-animator mouse-press-event mouse-press) (ev)
  (format t "Mouse press event: ~a ~a ~a~%" (q+:x ev) (q+:y ev) (q+:button ev)))

(define-override (grow-life-animator mouse-release-event mouse-release) (ev)
  (format t "Mouse press event: ~a ~a ~a~%" (q+:x ev) (q+:y ev) (q+:button ev)))

(define-override (grow-life-animator mouse-move-event mouse-move) (ev)
  (format t "Mouse press event: ~a ~a~%" (q+:x ev) (q+:y ev)))

(define-widget main-window (QMainWindow)
  ())

(define-override (main-window close-event) (ev)
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))

(define-subwidget (main-window scene-controller) (make-instance 'grow-life-animator)
  "The central controller-widget.")


(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) scene-controller)
  (q+:set-focus scene-controller))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))
