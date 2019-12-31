(defpackage sample-pixijs-on-cl/sample/utils
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :use-this-package-as-sample
           :get-global
           :set-global
           :get-default-stage))
(in-package :sample-pixijs-on-cl/sample/utils)

(enable-ps-experiment-syntax)

;; --- global info --- ;;

(defvar.ps+ *global-table* (make-hash-table))

(defun.ps+ get-global (key)
  (gethash key *global-table*))

(defun.ps+ set-global (key value)
  (setf (gethash key *global-table*) value))

(defsetf.ps+ get-global (key) (value)
  `(set-global ,key ,value))

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

;; --- common processes --- ;;

(defun.ps+ init-common ()
  (init-stats))

(defun.ps+ update-common (delta)
  (declare (ignore delta))
  (update-stats))

;; --- default variables --- ;;

(defun.ps+ get-default-stage ()
  (get-global :stage))

;; --- macro --- ;;

(defun.ps init-game-loop (app update-fn)
  (app.ticker.add update-fn))

(defmacro use-this-package-as-sample (&key
                                        (init-func (intern "INIT" *package*))
                                        (update-func (intern "UPDATE" *package*)))
  `(progn
     (defun ,(intern "OUTPUT-JS-CODE" *package*) (stream)
       (princ
        (pse:with-use-ps-pack (:this)
          (let* ((param (create :width 256 :height 256))
                 (app (new (#j.PIXI.Application# param))))
            (document.body.append-child app.view)
            (set-global :stage app.stage)
            (set-global :app app)
            (init-common)
            (funcall ,init-func)
            (init-game-loop app (lambda (delta)
                                  (update-common delta)
                                  (funcall ,update-func delta)))))
        stream))))
