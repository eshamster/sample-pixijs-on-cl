(defpackage sample-pixijs-on-cl/sample/simple
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-pixijs-on-cl/sample/utils
                :use-this-package-as-sample
                :get-global
                :set-global
                :get-default-stage))
(in-package :sample-pixijs-on-cl/sample/simple)

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

