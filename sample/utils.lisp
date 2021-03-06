(defpackage sample-pixijs-on-cl/sample/utils
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :use-this-package-as-sample
           :get-global
           :set-global
           :get-default-stage
           :get-dat-gui
           :get-screen-size
           ;; graphics
           :make-solid-rect
           :make-wired-rect
           :make-solid-circle
           :make-text
           :add-graphics
           :remove-graphics
           :set-model-xy))
(in-package :sample-pixijs-on-cl/sample/utils)

(enable-ps-experiment-syntax)

;; --- global table --- ;;

(defvar.ps+ *global-table* (make-hash-table))

(defun.ps+ get-global (key)
  (gethash key *global-table*))

(defun.ps+ set-global (key value)
  (setf (gethash key *global-table*) value))

(defsetf.ps+ get-global (key) (value)
  `(set-global ,key ,value))

;; --- screen size --- ;;

(defvar.ps+ *screen-width* 640)
(defvar.ps+ *screen-height* 480)

(defun.ps+ get-screen-size ()
  (values *screen-width* *screen-height*))

;; --- stats --- ;;

(defun.ps init-stats ()
  (let* ((stats (new (-stats)))
         (elem stats.dom-element)
         (style elem.style))
    (setf style.position "fixed"
          style.left "5px"
          style.top "5px")
    (document.body.append-child elem)
    (set-global :stats stats)))

(defun.ps update-stats ()
  ((@ (get-global :stats) update)))

;; --- dat.GUI --- ;;

(defun.ps init-dat-gui ()
  (let ((gui (new (#j.dat.GUI#))))
    (set-global :dat-gui gui)))

(defun.ps+ get-dat-gui ()
  (get-global :dat-gui))

;; --- common processes --- ;;

(defun.ps+ init-common ()
  (init-stats)
  (init-dat-gui))

(defun.ps+ update-common (delta)
  (declare (ignore delta))
  (update-stats))

;; --- default variables --- ;;

(defun.ps+ get-default-stage ()
  (get-global :stage))

;; --- initializer --- ;;

(defun.ps init-game-loop (app update-fn)
  (app.ticker.add update-fn))

(defun.ps init-app ()
  (let* ((param (create :width *screen-width* :height *screen-height*))
         (app (new (#j.PIXI.Application# param))))
    ;; TODO: Resize when window is resized.
    (let* ((style app.renderer.view.style)
           (scale (min (/ window.inner-width *screen-width*)
                       (/ window.inner-height *screen-height*)))
           (width (* *screen-width* scale))
           (height (* *screen-height* scale)))
      (setf style.width (+ width "px")
            style.height (+ height "px")
            style.position "absolute"
            style.left (+ (/ (- window.inner-width width) 2) "px")
            style.top (+ (/ (- window.inner-height height) 2) "px")))
    app))

(defmacro use-this-package-as-sample (&key
                                        (init-func (intern "INIT" *package*))
                                        (update-func (intern "UPDATE" *package*)))
  `(progn
     (defun ,(intern "OUTPUT-JS-CODE" *package*) (stream)
       (princ
        (pse:with-use-ps-pack (:this)
          (let* ((app (init-app)))
            (document.body.append-child app.view)
            (set-global :stage app.stage)
            (set-global :app app)
            (init-common)
            (funcall ,init-func)
            (init-game-loop app (lambda (delta)
                                  (update-common delta)
                                  (funcall ,update-func delta)))))
        stream))))

;; --- graphics --- ;;

(defun.ps make-solid-rect (&key width height color (alpha 1))
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.begin-fill color alpha)
    (rect.draw-rect 0 0 width height)
    (rect.end-fill)
    rect))

(defun.ps make-wired-rect (&key width height color (line-width 1))
  (let ((rect (new (#j.PIXI.Graphics#))))
    (rect.line-style line-width color 1)
    (rect.draw-rect 0 0 width height)
    rect))

(defun.ps make-solid-circle (&key r color (alpha 1))
  (let ((circle (new (#j.PIXI.Graphics#))))
    (circle.begin-fill color alpha)
    (circle.draw-circle 0 0 r)
    (circle.end-fill)
    circle))

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

(defun.ps remove-graphics (graphics &optional (stage (get-default-stage)))
  (assert stage)
  (stage.remove-child graphics))

(defun.ps set-model-xy (model x y)
  (setf model.x x
        model.y y))
