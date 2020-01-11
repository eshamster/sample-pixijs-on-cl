#|
  This file is a part of sample-pixijs-on-cl project.
  Copyright (c) 2018 eshamster (hamgoostar@gmail.com)
|#

#|
  Sample program to try pixi.js on Common Lisp (Parenscript)

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem sample-pixijs-on-cl
  :version "0.1"
  :class :package-inferred-system
  :author "eshamster"
  :license "MIT"
  :depends-on (:parenscript
               :ps-experiment
               :alexandria
               :ningle
               :cl-markup
               :clack
               :sample-pixijs-on-cl/main)
  :description "Sample program to try pixi.js on Common Lisp (Parenscript)"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sample-pixijs-on-cl/t))))

(defsystem sample-pixijs-on-cl/t
  :class :package-inferred-system
  :depends-on (:ps-experiment-test
               :rove
               "ps-experiment/t/test-utils"
               "sample-pixijs-on-cl/t/main")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
