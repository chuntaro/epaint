;;; epaint.el --- A simple paint tool for Emacs      -*- lexical-binding: t; -*-

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

(require 'epaint-canvas)

(defgroup epaint nil
  "A simple paint tool for Emacs."
  :prefix "epaint-"
  :group 'tools)

(defcustom epaint-buffer-name "*EPaint*"
  "The basename used for EPaint buffers."
  :type 'string
  :group 'epaint)

;;; Major Mode

(defvar-local epaint-canvas nil)

(defun epaint-canvas-down-mouse-1 (ev)
  (interactive "@e")
  (epaint-down-mouse-1 epaint-canvas ev))

(defun epaint-canvas-clear-image ()
  (interactive)
  (epaint-clear-image epaint-canvas t))

(defun epaint-canvas-set-pen-type-erase ()
  (interactive)
  (epaint-set-pen-type-erase epaint-canvas))

(defun epaint-canvas-rotate-color ()
  (interactive)
  (epaint-rotate-color epaint-canvas))

(defun epaint-canvas-set-pen-size (pen-size)
  (interactive "P")
  (unless pen-size
    (let ((key last-command-event))
      (setq pen-size (cond ((= key ?0)
                            (1- (* 10 2)))
                           ((and (>= key ?1) (<= key ?9))
                            (1- (* (- key ?0) 2)))))))
  (when pen-size
    (epaint-set-pen-size epaint-canvas pen-size)))

(defun epaint-canvas-undo ()
  (interactive)
  (epaint-undo epaint-canvas))

(defun epaint-canvas-redo ()
  (interactive)
  (epaint-undo epaint-canvas t))

(defun epaint-canvas-benchmarks ()
  (interactive)
  (epaint-benchmarks epaint-canvas))

(defvar epaint-mode-map
  (let ((map (make-keymap)))
    (define-key map [down-mouse-1] 'epaint-canvas-down-mouse-1)
    (define-key map "B"            'epaint-canvas-benchmarks)
    (define-key map "C"            'epaint-canvas-clear-image)
    (define-key map "E"            'epaint-canvas-set-pen-type-erase)
    (define-key map "R"            'epaint-canvas-rotate-color)
    (define-key map "u"            'epaint-canvas-undo)
    (define-key map "\C-r"         'epaint-canvas-redo)
    map)
  "Keymap for epaint major mode.")

(defun epaint-add-shape-plugin (key symbol-shape draw-func &optional need-restore need-connect)
  (epaint-draw-function-add symbol-shape draw-func need-restore need-connect)
  (define-key epaint-mode-map key (lambda ()
                                    (interactive)
                                    (epaint-change-shape epaint-canvas symbol-shape))))

(define-derived-mode epaint-mode nil "EPaint"
  "Major mode for editing X BitMap file with mouse."
  (unless (display-images-p)
    (error "Display does not support images"))

  ;; (insert "test test test\n")
  (setq epaint-canvas (make-instance 'epaint-canvas-class :point (point)))
  ;; (insert ?\n)
  ;; (insert "test test test\n")

  (epaint-setup-paint-mode epaint-canvas)

  (dolist (num (number-sequence 0 9))
    (define-key epaint-mode-map (number-to-string num) 'epaint-canvas-set-pen-size))

  (epaint-add-shape-plugin "f" 'freehand  'epaint-draw-function-freehand)
  (epaint-add-shape-plugin "c" 'circle    'epaint-draw-function-circle    t)
  (epaint-add-shape-plugin "e" 'ellipse   'epaint-draw-function-ellipse   t)
  (epaint-add-shape-plugin "r" 'rectangle 'epaint-draw-function-rectangle t)
  (epaint-add-shape-plugin "l" 'line      'epaint-draw-function-line      t)
  (epaint-add-shape-plugin "a" 'arrow     'epaint-draw-function-arrow     t)
  ;; (epaint-add-shape-plugin "p" 'polyline  'epaint-draw-function-line      t t)
  (epaint-change-shape epaint-canvas 'freehand)

  (epaint-debug 0
    (with-slots (bitmap width height) epaint-canvas
      (epaint-draw-thick-circle-with-radius bitmap width height t 3 9 100 100))

    (epaint-change-shape epaint-canvas 'arrow)
    (epaint-canvas-set-pen-size 19))

  (when buffer-file-name
    (epaint-read-file epaint-canvas buffer-file-name)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xbm\\'" . epaint-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ppm\\'" . epaint-mode))

;;;###autoload
(defun epaint (&optional filename)
  (interactive "P")
  (cl-assert epaint-buffer-name)
  (let ((buf (cond ((numberp filename)
		    (get-buffer-create (format "%s<%d>"
					       epaint-buffer-name
					       filename)))
		   (filename
		    (generate-new-buffer epaint-buffer-name))
		   (t
		    (get-buffer-create epaint-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'epaint-mode)
      (epaint-mode))

    (when (and filename (stringp filename))
      (epaint-read-file epaint-canvas filename))

    (epaint-debug 1
      (set-frame-position nil 250 250))

    (epaint-debug 0
      (let ((frame (select-frame (make-frame))))
        (set-frame-position frame 1000 250)
        (switch-to-buffer "*Messages*")))

    buf))

(provide 'epaint)
;;; epaint.el ends here
