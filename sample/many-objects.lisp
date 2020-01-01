(defpackage sample-pixijs-on-cl/sample/many-objects
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
(in-package :sample-pixijs-on-cl/sample/many-objects)

(enable-ps-experiment-syntax)
(use-this-package-as-sample)

;; --- graphics --- ;;

(defun.ps make-solid-circle (&key r color)
  (let ((circle (new (#j.PIXI.Graphics#))))
    (circle.begin-fill color)
    (circle.draw-circle 0 0 r)
    (circle.end-fill)
    circle))

(defun.ps add-graphics (graphics &optional (stage (get-default-stage)))
  (assert stage)
  (stage.add-child graphics))

(defun.ps remove-graphics (graphics &optional (stage (get-default-stage)))
  (assert stage)
  (stage.remove-child graphics))

;; --- params --- ;;

(defvar.ps+ *next-num-circles* 100)
(defvar.ps+ *circle-speed* 4)
(defvar.ps+ *circle-r* 10)
(defvar.ps+ *circle-color* #xaa88aa)

;; --- circle --- ;;

(defstruct.ps+ circle
    model
  speed-x
  speed-y)

(defstruct.ps+ circle-manager
    (enable-list (list))
  (disable-list (list)))

(defun.ps+ init-circle-manager ()
  (set-global :circle-manager (make-circle-manager)))

(defun.ps+ get-circle-manager ()
  (get-global :circle-manager))

(defun.ps rand1 ()
  (random))

(defun.ps add-circle (x y &optional (manager (get-circle-manager)))
  (let* ((disable-list (circle-manager-disable-list manager))
         (head (car disable-list)))
    (flet ((retrieve-disabled-circle ()
             (setf (circle-manager-disable-list manager)
                   (cdr disable-list))
             head)
           (make-new-circle ()
             (make-circle :model (make-solid-circle :r *circle-r* :color *circle-color*))))
      (let* ((circle (if head
                         (retrieve-disabled-circle)
                         (make-new-circle)))
             (model (circle-model circle))
             (angle (* 2 PI (rand1))))
        (setf model.x x
              model.y y)
        (setf (circle-speed-x circle) (* *circle-speed* (cos angle))
              (circle-speed-y circle) (* *circle-speed* (sin angle)))
        (add-graphics model)
        (push circle (circle-manager-enable-list manager))
        circle))))

(defun.ps+ remove-circle (&optional (manager (get-circle-manager)))
  (let* ((enable-list (circle-manager-enable-list manager))
         (disable-list (circle-manager-disable-list manager))
         (head (car enable-list)))
    (unless head
      (return-from remove-circle))
    (setf (circle-manager-enable-list manager)
          (cdr enable-list))
    (push head disable-list)
    (remove-graphics (circle-model head))))

(defun.ps+ adjust-circle-num (&optional (manager (get-circle-manager)))
  (let ((num (length (circle-manager-enable-list manager))))
    (cond ((< num *next-num-circles*)
           (multiple-value-bind (sw sh) (get-screen-size)
             (dotimes (i (- *next-num-circles* num))
               (add-circle (/ sw 2) (/ sh 2) manager))))
          ((> num *next-num-circles*)
           (dotimes (i (- num *next-num-circles*))
             (remove-circle manager))))))

(defun.ps+ update-circles (&optional (manager (get-circle-manager)))
  (adjust-circle-num)
  (dolist (circle (circle-manager-enable-list manager))
    (update-one-circle circle)))

(defun.ps update-one-circle (circle)
  (let ((model (circle-model circle))
        (speed-x (circle-speed-x circle))
        (speed-y (circle-speed-y circle)))
    (incf model.x speed-x)
    (incf model.y speed-y)
    (multiple-value-bind (sw sh) (get-screen-size)
      (when (< (- model.x *circle-r*) 0)
        (setf model.x (- (* 2 *circle-r*) model.x)
              (circle-speed-x circle) (* -1 speed-x)))
      (when (>= (+ model.x *circle-r*) sw)
        (setf model.x (- (* 2 sw)
                         (+ model.x (* 2 *circle-r*)))
              (circle-speed-x circle) (* -1 speed-x)))
      (when (< (- model.y *circle-r*) 0)
        (setf model.y (- (* 2 *circle-r*) model.y)
              (circle-speed-y circle) (* -1 speed-y)))
      (when (>= (+ model.y *circle-r*) sh)
        (setf model.y (- (* 2 sh)
                         (+ model.y (* 2 *circle-r*)))
              (circle-speed-y circle) (* -1 speed-y))))))

;; --- init and update --- ;;

(defun.ps init ()
  (let ((gui (get-dat-gui)))
    (chain gui
      (add (create num-objects *next-num-circles*) "numObjects" 100 5000)
      (step 100)
      (on-change (lambda (val)
                   (setf *next-num-circles* val)))))
  (init-circle-manager))

(defun.ps update (delta)
  (declare (ignorable delta))
  (update-circles))
