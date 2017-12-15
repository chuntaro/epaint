;;; codec-xbm.el --- Codec for XBM                   -*- lexical-binding: t; -*-

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

(defun epaint-read-xbm (drawable)
  (let ((width 0)
        (height 0)
        (count 0)
        (foreground (epaint-gc-set-color (make-epaint-gc) (epaint-foreground-color)))
        (background (epaint-gc-set-color (make-epaint-gc) (epaint-background-color))))
    (when (search-forward-regexp "^#define [0-9a-zA-Z_]+_width \\([0-9]+\\)$" nil t)
      (setq width (string-to-number (buffer-substring (match-beginning 1) (match-end 1))))
      (when (search-forward-regexp "^#define [0-9a-zA-Z_]+_height \\([0-9]+\\)$" nil t)
        (setq height (string-to-number (buffer-substring (match-beginning 1) (match-end 1))))
        (epaint-drawable-regenerate drawable width height)
        (when (search-forward-regexp "^static unsigned char [0-9a-zA-Z_]+_bits\\[\\] = {$" nil t)
          (cl-loop for i from 0 below (* width height) by 8
                   while (search-forward-regexp "0x\\([0-9a-fA-F][0-9a-fA-F]\\)" nil t)
                   for hex = (buffer-substring (match-beginning 1) (match-end 1))
                   for bits = (string-to-number hex 16)
                   do (cl-loop for j from 0 below 8
                               for bit = (logand bits (lsh 1 j)) do
                               (epaint-set-pixel-linear drawable
                                                        (if (zerop bit) background foreground)
                                                        (+ i j))
                               (cl-incf count)))
          t)))))

(defun epaint-write-xbm (filename drawable)
  (let ((width (epaint--width drawable))
        (height (epaint--height drawable))
        (id (replace-regexp-in-string "[^0-9a-zA-Z_]" "_" (file-name-base filename)))
        (bg-color (epaint-background-color)))
    (insert (format "#define %s_width %s\n" id width)
            (format "#define %s_height %s\n" id height)
            (format "static unsigned char %s_bits[] = {\n" id))
    (cl-loop with endi = (1- (* width height))
             with ncols = 12
             for i from 0 to endi
             for b = (if (not (equal (epaint-get-pixel-linear drawable i)
                                     bg-color))
                         1 0)
             for s = (% i 8)
             for c = (% (/ i 8) ncols)
             for bits = b then (logior bits (lsh b s))
             when (= s 7)
             do
             (insert (if (zerop c) "   " " ")
                     (format "0x%02x" bits)
                     (if (/= i endi)
                         (if (= c (1- ncols)) ",\n" ",")
                       " };\n"))
             (setq bits 0))))

(provide 'codec-xbm)
;;; codec-xbm.el ends here
