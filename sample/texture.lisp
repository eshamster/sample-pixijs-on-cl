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

(defstruct.ps+ uv-rect (x 0) (y 0) (width 1) (height 1))

(defstruct.ps+ sprite-info
    name x y width height process-sprite
    (uv (make-uv-rect)))

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
           (let ((uv (sprite-info-uv info)))
             (init-texture-uv tex
                              (uv-rect-x uv)
                              (uv-rect-y uv)
                              (uv-rect-width uv)
                              (uv-rect-height uv)))
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
  (chain (new (#j.PIXI.Loader#)
              (add name path))
    (load (lambda ()))))

;; PIXI.loader.resources['lala'].t﻿exture.baseText﻿ure
(defun.ps get-texture (name)
  (let ((tex (gethash name #j.PIXI.utils.TextureCache#)))
    (when tex
      (new (#j.PIXI.Texture# tex)))))

(defun.ps make-sprite (tex)
  (new (#j.PIXI.Sprite# tex)))

(defun.ps set-sprite-rect (sprite x y width height)
  (setf sprite.x x
        sprite.y y
        sprite.width width
        sprite.height height))

(defun.ps init-texture-uv (tex ux uy uw uh)
  ;; This assumes that the frame has an initial values, that is,
  ;; the width and height are same to the image's.
  (let* ((frame tex.frame)
         (width frame.width)
         (height frame.height))
    (setf tex.frame (new (#j.PIXI.Rectangle# (* width ux)
                                             (* height uy)
                                             (* width uw)
                                             (* height uh))))))

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
  (load-texture "img/multiple_image.png" "multiple")
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
                                (aset-sprite-rotation sprite (- it 0.02)))))
    (register-sprite-info
     manager (make-sprite-info
              :name "multiple" :x 300 :y 300 :width 100 :height 100
              :uv (make-uv-rect :width 0.5)
              :process-sprite (lambda (sprite) (declare (ignore sprite)))))
    (register-sprite-info
     manager (make-sprite-info
              :name "multiple" :x 380 :y 380 :width 100 :height 100
              :uv (make-uv-rect :x 0.5 :width 0.5)
              :process-sprite (lambda (sprite) (declare (ignore sprite)))))))

(defun.ps+ update (delta)
  (declare (ignorable delta))
  (process-sprite-info-manager (get-global :manager)))

