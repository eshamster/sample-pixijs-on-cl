(defpackage sample-pixijs-on-cl/sample/utils
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :use-this-package-as-sample
           :get-global
           :set-global
           :get-default-stage
           :get-dat-gui
           :get-screen-size))
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

;; --- macro --- ;;

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
