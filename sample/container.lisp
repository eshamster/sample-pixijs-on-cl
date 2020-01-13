(defpackage sample-pixijs-on-cl/sample/container
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-pixijs-on-cl/sample/utils
                :use-this-package-as-sample
                :get-global
                :set-global
                :make-solid-rect
                :make-wired-rect
                :make-text
                :add-graphics
                :get-dat-gui))
(in-package :sample-pixijs-on-cl/sample/container)

(enable-ps-experiment-syntax)
(use-this-package-as-sample)

;; --- init and update --- ;;

(defun.ps add-to-container (container graphics)
  (container.add-child graphics))

(defun.ps init-gui ()
  (let ((gui (get-dat-gui))
        (table (create scale 1
                       offset-x 0
                       offset-y 0)))
    (set-global :params table)
    (chain gui
      (add table "scale" 0.5 2)
      (step 0.1))
    (chain gui
      (add table "offsetX" -50 100)
      (step 1))
    (chain gui
      (add table "offsetY" -50 100)
      (step 1))))

(defun.ps init ()
  (let ((container (new (#j.PIXI.Container#))))
    (add-graphics container)
    (set-global :container container)
    ;; solid rect
    (let ((rect (make-solid-rect :width 60 :height 30 :color #xff00ff)))
      (setf rect.x 100
            rect.y 100)
      (add-to-container container rect))
    ;; wired rect
    (let ((rect (make-wired-rect :width 30 :height 60 :color #x00ff00 :line-width 1)))
      (setf rect.x 100
            rect.y 100)
      (add-to-container container rect))
    ;; text
    (let ((text (make-text "斑鳩" :color #xffffff)))
      (setf text.x 50
            text.y 50)
      (add-to-container container text)
      (setf (get-global :text) text)))
  (setf (get-global :count) 0)
  ;; init GUI
  (init-gui))

(defun.ps update (delta)
  (declare (ignorable delta))
  (setf (get-global :count) (1+ (get-global :count)))
  (let ((text (get-global :text)))
    (setf text.text (get-global :count)))
  (let* ((container (get-global :container))
         (params (get-global :params))
         (scale (gethash "scale" params))
         (offset-x (gethash "offsetX" params))
         (offset-y (gethash "offsetY" params))))
    (setf container.scale.x scale
          container.scale.y scale)
    (setf container.x offset-x
          container.y offset-y))

