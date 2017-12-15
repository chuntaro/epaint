;;; epaint-internal-format-xbm.el --- Internal format for XBM -*- lexical-binding: t; -*-

;; Copyright (C) 2017  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>

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

(require 'epaint-context)

(defun epaint-foreground-color ()
  t)

(defun epaint-background-color ()
  nil)

(defun epaint-lookup-color (color)
  (if (cl-typecase color
        (vector (>= 128 (+ (* 0.299 (aref color 0))
                           (* 0.587 (aref color 1))
                           (* 0.114 (aref color 2)))))
        (string (string= color "black"))
        (otherwise color))
      (epaint-foreground-color)
    (epaint-background-color)))

(defun epaint-drawable-create (width height)
  (let ((drawable (make-epaint-drawable)))
    (setf (epaint--bitmap drawable) (make-bool-vector (* width height) nil)
          (epaint--offset drawable) 0
          (epaint--width drawable) width
          (epaint--height drawable) height)
    drawable))

(defsubst epaint-bitmap-clear (bitmap _offset color)
  (fillarray bitmap color))

(defsubst epaint-bitmap-get-pixel-linear (bitmap _offset i)
  (aref bitmap i))

(defsubst epaint-bitmap-set-pixel-linear (bitmap _offset i color)
  (aset bitmap i color))

(provide 'epaint-internal-format-xbm)
;;; epaint-internal-format-xbm.el ends here
