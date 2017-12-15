;;; codec-pbm.el --- Codec for PBM                   -*- lexical-binding: t; -*-

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

(require 'epaint-draw)
(require 'epaint-context)

(defun epaint-read-ppm (drawable)
  (let ((state 'magic-value)
        (width 0)
        (height 0)
        (gc (make-epaint-gc)))
    (when (cl-block nil
            (while (search-forward-regexp "^\\(.*\\)$" nil t)
              (let ((line (buffer-substring (match-beginning 1) (match-end 1))))
                (if (zerop (length line))
                    (cl-return)
                  (when (/= ?# (aref line 0))
                    (cl-case state
                      (magic-value
                       (if (string= line "P6")
                           (setf state 'size-value)
                         (cl-return)))
                      (size-value
                       (let ((size (split-string line " " t)))
                         (setf width (string-to-number (car size))
                               height (string-to-number (cadr size))))
                       (setf state 'max-value))
                      (max-value
                       (cl-return (string= line "255")))))))))
      (forward-char)
      (epaint-drawable-regenerate drawable width height)
      (cl-loop with point = (point)
               for i from 0 below (* width height)
               for pos = (+ point (* i 3))
               for color = (vconcat (string-to-unibyte (buffer-substring-no-properties
                                                        pos (+ pos 3))))
               do
               (epaint-gc-set-color gc color)
               (epaint-set-pixel-linear drawable gc i))
      t)))

(defun epaint-write-ppm (_filename drawable)
  (set-buffer-multibyte nil)
  (set-buffer-file-coding-system 'no-conversion)
  (let ((width (epaint--width drawable))
        (height (epaint--height drawable)))
    (insert "P6\n"
            (format "%d %d\n" width height)
            "255\n")
    (cl-loop for i from 0 below (* width height)
             for color = (epaint-lookup-color (epaint-get-pixel-linear drawable i))
             do (insert (concat color)))))

(provide 'codec-pbm)
;;; codec-pbm.el ends here
