(defpackage sample-pixijs-on-cl/server
  (:use :cl
        :cl-markup
        :sample-pixijs-on-cl/js)
  (:export :start
           :stop))
(in-package :sample-pixijs-on-cl/server)

;; --- Definitions about directories --- ;;

(defvar *script-dir*
  (merge-pathnames "static/"
                   (asdf:component-pathname
                    (asdf:find-system :sample-pixijs-on-cl))))

(defvar *js-dir*
  (merge-pathnames "js/" *script-dir*))

(defvar *js-main-file*
  (merge-pathnames "main.js" *js-dir*))

;; --- Make js main file --- ;;

(defun make-js-main-file (sample-name)
  (with-open-file (out (merge-pathnames (format nil "sample-~A.js" sample-name)
                                        *js-dir*)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (funcall (intern "OUTPUT-JS-CODE"
                     (find-package (format nil "SAMPLE-PIXIJS-ON-CL/SAMPLE/~A"
                                           (string-upcase sample-name))))
             out)))

;; --- Server --- ;;

(defvar *app* (make-instance 'ningle:<app>))

(defvar *server* nil)

(defmacro with-cl-markup (&body body)
  (let ((g-str (gensym)))
    `(with-output-to-string (,g-str)
       (let ((cl-markup:*output-stream* ,g-str))
         ,@body))))

(setf (ningle:route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (with-cl-markup
          (html5 (:head
                  (:title "Samples of Pixi.js on Common Lisp (Parenscript)")
                  (:link :rel "stylesheet" :type "text/css" :href "css/style.css" nil))
                 (:body
                  (:div :id "panel"
                        (:div
                         :id "panel-content"
                         (:div
                          (:a :href "https://github.com/eshamster/sample-pixijs-on-cl" "sample-pixijs-on-cl"))
                         (:ul
                          :id "sample-list"
                          (dolist (name '("simple"
                                          "many-objects"))
                            (markup (:li :class "sample-list-element"
                                         (:a :href (format nil "/sample-~A" name)
                                             :target "viewer"
                                             name)
                                         " ("
                                         (:a :href (format nil "https://github.com/eshamster/sample-pixijs-on-cl/blob/master/sample/~A.lisp" name)
                                             :target "_blank"
                                             "code")
                                         ")"))))))
                  (:iframe :id "viewer" :name "viewer" nil))))))

(setf (ningle:route *app* "/sample-*" :method :GET)
      (lambda (params)
        (let ((name (getf (car params) :splat)))
          ;; TODO: error response when the name is not exist
          (make-js-main-file name)
          (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "sample-pixijs-on-cl")
                    (:script :src "https://cdnjs.cloudflare.com/ajax/libs/pixi.js/5.2.0/pixi.js" nil)
                    (:script :src "https://cdnjs.cloudflare.com/ajax/libs/stats.js/r16/Stats.js" nil)
                    (:script :src "https://cdnjs.cloudflare.com/ajax/libs/dat-gui/0.7.6/dat.gui.js" nil))
                   (:body
                    (:script :src (format nil "js/sample-~A.js" name) nil))))))))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

(defun start (&key (port 5000) (address "0.0.0.0"))
  (stop)
  (setf *server*
        (clack:clackup
         (lack:builder
          (:static :path (lambda (path)
                           (print path)
                           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)"
                                           path)
                               path
                               nil))
                   :root *script-dir*)
          *app*)
         :port port
         :address address)))
