(defpackage sample-pixijs-on-cl/sample/z-index
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-pixijs-on-cl/sample/utils
                :use-this-package-as-sample
                :get-global
                :set-global
                :get-default-stage
                :get-dat-gui
                :get-screen-size))
(in-package :sample-pixijs-on-cl/sample/z-index)

(enable-ps-experiment-syntax)
(use-this-package-as-sample)

;; --- graphics --- ;;

(defun.ps make-solid-rect (&key width height color)
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.begin-fill color)
    (rect.draw-rect 0 0 width height)
    (rect.end-fill)
    rect))

(defun.ps make-wired-rect (&key width height color (line-width 1))
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.line-style line-width color 1)
    (rect.draw-rect 0 0 width height)
    rect))

(defun.ps make-text (text &key
                          (font-family "Arial")
                          (font-size 32)
                          (color "black")
                          (other-opts (make-hash-table)))
  (let ((opts (make-hash-table)))
    (dolist (pair (list (list "fontFamily" font-family)
                        (list "fontSize" font-size)
                        (list "fill" color)))
      (setf (gethash (car pair) opts)
            (cadr pair)))
    (maphash (lambda (k v)
               (setf (gethash k opts) v))
             other-opts)
    (let ((style (new (#j.PIXI.TextStyle# opts))))
      (new (#j.PIXI.Text# text style)))))

(defun.ps add-graphics (graphics &optional (stage (get-default-stage)))
  (assert stage)
  (stage.add-child graphics))

;; --- params --- ;;

(defvar.ps+ *num-objects* 4)
(defvar.ps+ *small-is-front-p* t)
(defvar.ps+ *rect-length* 80)

;; --- object --- ;;

(defstruct.ps+ sort-object
    number
  number-model
  rect-model
  frame-model)

(defstruct.ps+ sort-object-manager
    (lst (list)))

(defun.ps+ init-sort-object-manager ()
  (let ((so-manager (make-sort-object-manager)))
    (dotimes (i *num-objects*)
      (push
       (add-sort-object (+ 100 (* i 0.5 *rect-length*))
                        (+ 100 (* i 0.5 *rect-length*))
                        i)
       (sort-object-manager-lst so-manager)))
    (set-global :sort-object-manager so-manager)))

(defun.ps+ get-sort-object-manager ()
  (get-global :sort-object-manager))

(defun.ps add-sort-object (x y number &optional (manager (get-sort-object-manager)))
  (let ((rect (make-solid-rect :width *rect-length*
                               :height *rect-length*
                               :color #x44ccccc)))
    (setf rect.x x
          rect.y y)
    (add-graphics rect)
    (let ((frame (make-wired-rect :width *rect-length*
                                  :height *rect-length*
                                  :color #xcc44cc
                                  :line-width 3)))
      (setf frame.x x
            frame.y y)
      (add-graphics frame)
      (let ((text (make-text number
                             :font-size *rect-length*)))
        (setf text.x (+ x (/ *rect-length* 4))
              text.y y)
        (add-graphics text)
        (make-sort-object :number number
                          :number-model text
                          :rect-model rect
                          :frame-model frame)))))

(defun.ps update-sort-objects (&optional (manager (get-sort-object-manager)))
  (dolist (so (sort-object-manager-lst manager))
    (let* ((num (sort-object-number so))
           (z-index (* num 10)))
      (when *small-is-front-p*
        (setf z-index (* -1 z-index)))
      (let ((r-model (sort-object-rect-model so))
            (f-model (sort-object-frame-model so))
            (n-model (sort-object-number-model so)))
        (setf r-model.z-index z-index
              f-model.z-index (1+ z-index)
              n-model.z-index (1+ z-index))))))

(defun.ps sort-objects (&optional (stage (get-default-stage)))
  (stage.children.sort
   (lambda (a b)
     (- a.z-index b.z-index))))

;; --- init and update --- ;;

(defun.ps init ()
  (let ((gui (get-dat-gui)))
    (chain gui
      (add (create small-is-front t) "smallIsFront")
      (on-change (lambda (val)
                   (setf *small-is-front-p* val)))))
  (init-sort-object-manager))

(defun.ps update (delta)
  (declare (ignorable delta))
  (update-sort-objects)
  (sort-objects))
