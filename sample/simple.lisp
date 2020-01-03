(defpackage sample-pixijs-on-cl/sample/simple
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-pixijs-on-cl/sample/utils
                :use-this-package-as-sample
                :get-global
                :set-global
                :get-default-stage
                :make-solid-rect
                :make-wired-rect
                :make-text
                :add-graphics))
(in-package :sample-pixijs-on-cl/sample/simple)

(enable-ps-experiment-syntax)
(use-this-package-as-sample)

;; --- init and update --- ;;

(defun.ps init ()
  ;; solid rect
  (let ((rect (make-solid-rect :width 60 :height 30 :color #xff00ff)))
    (setf rect.x 100
          rect.y 100)
    (add-graphics rect))
  ;; wired rect
  (let ((rect (make-wired-rect :width 30 :height 60 :color #x00ff00 :line-width 1)))
    (setf rect.x 100
          rect.y 100)
    (add-graphics rect))
  ;; text
  (let ((text (make-text "斑鳩" :color #xffffff)))
    (setf text.x 50
          text.y 50)
    (add-graphics text)
    (setf (get-global :text) text))
  (setf (get-global :count) 0))

(defun.ps update (delta)
  (declare (ignorable delta))
  (setf (get-global :count) (1+ (get-global :count)))
  (let ((text (get-global :text)))
    (setf text.text (get-global :count))))

