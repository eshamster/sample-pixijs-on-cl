(defpackage sample-pixijs-on-cl/sample/text
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-pixijs-on-cl/sample/utils
                :use-this-package-as-sample
                :get-global
                :set-global
                :make-wired-rect
                :make-text
                :add-graphics
                :set-model-xy))
(in-package :sample-pixijs-on-cl/sample/text)

(enable-ps-experiment-syntax)
(use-this-package-as-sample)

(defun.ps get-text-size (text-model)
  (values text-model.width text-model.height))

(defun.ps+ add-text-with-frame (text-model x y)
  (multiple-value-bind (width height) (get-text-size text-model)
    (let ((frame (make-wired-rect :width width :height height
                                  :color #x00ff00 :line-width 1)))
      (set-model-xy frame x y)
      (add-graphics frame)))
  (set-model-xy text-model x y)
  (add-graphics text-model))

(defun.ps+ add-aligned-text-list (&key text-list align base-x base-y)
  (let ((y base-y))
    (dolist (text text-list)
      (let ((text-model (make-text text :color #xffffff)))
        (multiple-value-bind (width height) (get-text-size text-model)
          (declare (ignore width))
          (add-aligned-text :text-model text-model :align align :base-x base-x :y y)
          (incf y height))))))

(defun.ps+ add-aligned-text (&key text-model align base-x y)
  (multiple-value-bind (width) (get-text-size text-model)
    (let ((x (ecase align
               (:left base-x)
               (:center (- base-x (/ width 2)))
               (:right (- base-x width)))))
      (set-model-xy text-model x y)))
  (add-graphics text-model))

;; --- init and update --- ;;

(defun.ps+ init ()
  (let ((text (make-text "text with frame" :color #xffffff)))
    (add-text-with-frame text 50 50))
  (add-aligned-text-list :text-list (list "1" "12" "123")
                         :align :right
                         :base-x 200
                         :base-y 200)
  (add-aligned-text-list :text-list (list "1" "12" "123")
                         :align :center
                         :base-x 350
                         :base-y 200)
  (add-aligned-text-list :text-list (list "1" "12" "123")
                         :align :left
                         :base-x 500
                         :base-y 200))

(defun.ps+ update (delta)
  (declare (ignorable delta)))

