;;; epaint-internal-format-pbm.el --- Internal format for PBM -*- lexical-binding: t; -*-

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
  [0 0 0])

(defun epaint-background-color ()
  [255 255 255])

(defconst epaint--color-value-max (car (color-values "#ffffff")))
(defconst epaint--color-value-divisor (/ (float epaint--color-value-max) 255))

(defun epaint-lookup-color (color)
  (cl-typecase color
    (vector color)
    (string (let ((values (color-values color)))
              (if (null values)
                  (epaint-foreground-color)
                (apply #'vector
                       (mapcar (lambda (value)
                                 (truncate value epaint--color-value-divisor))
                               values)))))
    (otherwise (if color
                   (epaint-foreground-color)
                 (epaint-background-color)))))

(defun epaint-drawable-create (width height)
  (let ((drawable (make-epaint-drawable))
        (header (concat "P6\n"
                        (number-to-string width)
                        " "
                        (number-to-string height)
                        "\n"
                        "255\n")))
    (setf (epaint--bitmap drawable) (encode-coding-string
                                     (concat header (make-string (* width height 3) 255))
                                     'no-conversion)
          (epaint--offset drawable) (length header)
          (epaint--width drawable) width
          (epaint--height drawable) height)
    drawable))

(defsubst epaint-bitmap-clear (bitmap offset color)
  (let ((len (length bitmap))
        (r (aref color 0))
        (g (aref color 1))
        (b (aref color 2)))
    (while (< offset len)
      (aset bitmap offset r) (cl-incf offset)
      (aset bitmap offset g) (cl-incf offset)
      (aset bitmap offset b) (cl-incf offset))))

(defsubst epaint-bitmap-get-pixel-linear (bitmap offset i)
  (let* ((r (+ offset (* i 3)))
         (g (1+ r))
         (b (1+ g)))
    (vector (aref bitmap r)
            (aref bitmap g)
            (aref bitmap b))))

(defsubst epaint-bitmap-set-pixel-linear (bitmap offset i color)
  (let* ((r (+ offset (* i 3)))
         (g (1+ r))
         (b (1+ g)))
    (aset bitmap r (aref color 0))
    (aset bitmap g (aref color 1))
    (aset bitmap b (aref color 2))))

(provide 'epaint-internal-format-pbm)
;;; epaint-internal-format-pbm.el ends here
