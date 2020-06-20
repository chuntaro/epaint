;;; epaint-canvas.el --- A simple paint tool for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  chuntaro <chuntaro@sakura-games.jp>

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Maintainer: chuntaro <chuntaro@sakura-games.jp>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: paint, mouse
;; URL: https://github.com/chuntaro/xxxxxx

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'subr-x)
(require 'cl-macs)

(defvar epaint-draw-function-table)
(defvar epaint-internal-format)

;;; Graphics Context

(cl-defstruct (epaint-drawable (:conc-name epaint--))
  bitmap
  offset
  width
  height)

(defmacro epaint-with-drawable (spec-list object &rest body)
  "Macro to directly access slots.

  (epaint-with-drawable (bitmap width height) drawable
    body)
  |
  v
  (let ((bitmap (epaint--bitmap drawable))
        (width (epaint--width drawable))
        (height (epaint--height drawable)))
    body)"
  (declare (indent 2))
  `(let ,(mapcar (lambda (entry)
                   (if (assq entry (cl-struct-slot-info 'epaint-drawable))
                       (list entry `(,(intern (concat "epaint--" (symbol-name entry))) ,object))
                     (warn "epaint-with-drawable: Unknown slot-name: `%s'" entry)))
                 spec-list)
     ,@body))

(defun epaint-drawable-create-image (drawable)
  (create-image (epaint--bitmap drawable) epaint-internal-format t
                :width (epaint--width drawable)
                :height (epaint--height drawable)
                ;; text (or nil), arrow, hand, vdrag, hdrag, modeline, hourglass
                :pointer 'arrow
                :foreground "black"
                :background "white"))

(defun epaint-drawable-regenerate (drawable width height)
  (let ((new (epaint-drawable-create width height)))
    (setf (epaint--bitmap drawable) (epaint--bitmap new)
          (epaint--width drawable) (epaint--width new)
          (epaint--height drawable) (epaint--height new)
          (epaint--offset drawable) (epaint--offset new))
    drawable))

(cl-defstruct (epaint-gc
               (:constructor nil)
               (:constructor make-epaint-gc
                             (&key
                              (color (epaint-lookup-color t))
                              &aux
                              (color (epaint-lookup-color color))
                              (shape 'freehand)
                              (draw-function (assq shape epaint-draw-function-table))
                              (draw-func (nth 1 draw-function))
                              (restore-p (not (null (nth 2 draw-function))))
                              (connect-p (not (null (nth 3 draw-function)))))))
  color
  (size 5)
  shape
  (current-x 0)
  (current-y 0)
  draw-func
  restore-p
  connect-p)

(defmacro epaint-with-gc (spec-list object &rest body)
  "Macro to directly access slots.

  (epaint-with-gc (color size shape) gc
    body)
  |
  v
  (let ((color (epaint-gc-color gc))
        (size (epaint-gc-size gc))
        (shape (epaint-gc-shape gc)))
    body)"
  (declare (indent 2))
  `(let ,(mapcar (lambda (entry)
                   (if (assq entry (cl-struct-slot-info 'epaint-gc))
                       (list entry `(,(intern (concat "epaint-gc-" (symbol-name entry))) ,object))
                     (warn "epaint-with-gc: Unknown slot-name: `%s'" entry)))
                 spec-list)
     ,@body))

(defun epaint-gc-set-color (gc color)
  (setf (epaint-gc-color gc) (epaint-lookup-color color))
  gc)

(defun epaint-gc-change-shape (gc shape-symbol)
  (when-let (draw-function (assq shape-symbol epaint-draw-function-table))
    (setf (epaint-gc-shape gc) shape-symbol
          (epaint-gc-draw-func gc) (nth 1 draw-function)
          (epaint-gc-restore-p gc) (not (null (nth 2 draw-function)))
          (epaint-gc-connect-p gc) (not (null (nth 3 draw-function)))))
  gc)

(defun epaint-gc-set-pen-size (gc pen-size)
  (setf (epaint-gc-size gc) pen-size)
  gc)

(provide 'epaint-context)
;;; epaint-context.el ends here
