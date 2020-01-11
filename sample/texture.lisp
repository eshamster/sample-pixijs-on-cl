(defpackage sample-pixijs-on-cl/sample/texture
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-pixijs-on-cl/sample/utils
                :use-this-package-as-sample
                :get-global
                :set-global
                :add-graphics)
  (:import-from :alexandria
                :with-gensyms))
(in-package :sample-pixijs-on-cl/sample/texture)

(enable-ps-experiment-syntax)
(use-this-package-as-sample)

(defstruct.ps+ sprite-info
    name x y width height process-sprite)

(defvar.ps+ *next-info-id* 0)

(defstruct.ps+ sprite-info-manager
    (uninit-info-table (make-hash-table))
  (process-list (list)))

(defun.ps+ process-sprite-info-manager (manager)
  (let ((inited-id-list (list)))
    (maphash
     (lambda (id info)
       (let ((tex (get-texture (sprite-info-name info))))
         (when tex
            (push id inited-id-list)
           (let ((sprite (make-sprite tex)))
             (set-sprite-rect sprite
                              (sprite-info-x info)
                              (sprite-info-y info)
                              (sprite-info-width info)
                              (sprite-info-height info))
             (add-graphics sprite)
             (let ((proc (sprite-info-process-sprite info)))
               (push (lambda ()
                       (funcall proc sprite))
                     (sprite-info-manager-process-list manager)))))))
     (sprite-info-manager-uninit-info-table manager))
    (dolist (id inited-id-list)
      (remhash id (sprite-info-manager-uninit-info-table manager))))
  (dolist (proc (sprite-info-manager-process-list manager))
    (funcall proc)))

(defun.ps+ register-sprite-info (manager info)
  (setf (gethash (incf *next-info-id*)
                 (sprite-info-manager-uninit-info-table manager))
        info))

(defun.ps load-texture (path name)
  (chain #j.PIXI.Loader.shared#
    (add name path)
    (load (lambda ()))))

(defun.ps get-texture (name)
  (gethash name #j.PIXI.utils.TextureCache#))

(defun.ps make-sprite (tex)
  (new (#j.PIXI.Sprite# tex)))

(defun.ps set-sprite-rect (sprite x y width height)
  (setf sprite.x x
        sprite.y y
        sprite.width width
        sprite.height height))

(defun.ps get-sprite-rotation (sprite)
  sprite.rotation)

(defun.ps set-sprite-rotation (sprite val)
  (setf sprite.rotation val))

(defmacro.ps+ aset-sprite-rotation (sprite val)
  (with-gensyms (g-sprite)
    `(let* ((,g-sprite ,sprite)
            (it (get-sprite-rotation ,g-sprite)))
       (set-sprite-rotation ,g-sprite ,val))))

(defun.ps set-sprite-anchor (sprite ux uy)
  (sprite.anchor.set ux uy))

;; --- init and update --- ;;

(defun.ps+ init ()
  (load-texture "img/sample.png" "sample")
  (let ((manager (make-sprite-info-manager)))
    (set-global :manager manager)
    (register-sprite-info
     manager (make-sprite-info
              :name "sample" :x 100 :y 100 :width 100 :height 100
              :process-sprite (lambda (sprite)
                                (aset-sprite-rotation sprite (+ it 0.02)))))
    (register-sprite-info
     manager (make-sprite-info
              :name "sample" :x 180 :y 180 :width 100 :height 100
              :process-sprite (lambda (sprite)
                                (set-sprite-anchor sprite 0.5 0.5)
                                (aset-sprite-rotation sprite (- it 0.02)))))))

(defun.ps+ update (delta)
  (declare (ignorable delta))
  (process-sprite-info-manager (get-global :manager)))

