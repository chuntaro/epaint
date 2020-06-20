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

(eval-and-compile
  (defvar epaint-internal-format 'pbm))

(require (intern (concat "epaint-internal-format-"
                         (symbol-name epaint-internal-format))))

(require 'eieio)
(require 'epaint-draw)
(require 'epaint-context)
(require 'epaint-history)

;;; Helpers

(defmacro epaint-with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))

(defun epaint-setup-paint-mode (canvas)
  (buffer-disable-undo)
  (setq show-trailing-whitespace nil
        buffer-read-only t
        truncate-lines t
        line-spacing 0
        cursor-type nil
        view-read-only nil
        write-contents-functions (lambda ()
                                   (epaint-write-file canvas
                                                      buffer-file-name)))
  (setq-local auto-hscroll-mode nil)
  (add-hook 'auto-save-hook
            (lambda ()
              (and (eq major-mode 'epaint-mode)
                   canvas
                   (epaint-write-back canvas buffer-file-name)))))

;;; Plug-in Functions

(defvar epaint-default-directory (file-name-directory
                                  (file-truename
                                   (expand-file-name
                                    (locate-library "epaint")))))

(defvar epaint-plug-in-default-directory (concat epaint-default-directory
                                                 "plug-ins/"))

(defun epaint-plug-in-load-all (&optional plug-ins-path)
  (let ((match (mapconcat (lambda (sfx)
                            (concat "\\" sfx "$"))
                          (get-load-suffixes)
                          "\\|"))
        (path (or plug-ins-path
                  epaint-plug-in-default-directory)))
    ;; (dolist (file (directory-files path t "\\.el$"))
    ;;   (byte-compile-file file))
    (mapcar (lambda (file)
              (let ((load-path (list path)))
                (require (intern file) nil t)))
            (delete-dups (mapcar #'file-name-base
                                 (directory-files path nil match))))))

(defun epaint-plug-in-codec-get (prefix filename &optional ext)
  (when (and prefix filename)
    (setq ext (or ext (file-name-extension filename) "xbm")
          filename (if (string= (downcase (file-name-extension filename t))
                                (concat "." ext))
                       filename
                     (concat (file-name-sans-extension filename) "." ext)))
    (let ((func (intern (format "epaint-%s-%s" prefix ext))))
      (when (fboundp func)
        (cl-values func filename ext)))))

(defun epaint-plug-in-codec-funcall (prefix ext &rest args)
  (let ((func (intern (format "epaint-%s-%s" prefix ext))))
    (when (fboundp func)
      (apply func args))))

;;; Canvas

(defclass epaint-canvas-class ()
  ((drawable   :initarg :drawable   :accessor drawable   :type epaint-drawable)
   (gc         :initarg :gc         :accessor gc         :type epaint-gc)
   (foreground :initarg :foreground :accessor foreground :type epaint-gc)
   (background :initarg :background :accessor background :type epaint-gc)
   (image      :initarg :image      :accessor image      :type list)
   (overlay    :initarg :overlay    :accessor overlay    :type overlay)
   (history    :initarg :history    :accessor history    :type epaint-history
               :initform (make-epaint-history))))

(cl-defmethod initialize-instance ((this epaint-canvas-class) &optional args)
  (require (intern (concat "epaint-internal-format-"
                           (symbol-name epaint-internal-format))))
  (epaint-plug-in-load-all)
  ;; (byte-compile-file (concat epaint-default-directory "epaint-draw.el") t)
  ;; (byte-compile-file (concat epaint-default-directory "epaint-canvas.el") t)
  (cl-call-next-method this)
  (let (gc-args)
    (dolist (prop '(:width :height))
      (when-let (value (plist-get args prop))
        (setf gc-args (plist-put gc-args prop value))))
    (setf (drawable this) (epaint-drawable-create 640 480)
          (foreground this) (apply #'make-epaint-gc (cl-list* :color t gc-args))
          (background this) (apply #'make-epaint-gc (cl-list* :color nil gc-args))
          (gc this) (foreground this))
    (epaint-create-image this :point (plist-get args :point))))

(cl-defmethod epaint-create-image ((this epaint-canvas-class) &key point)
  (setf (image this) (epaint-drawable-create-image (drawable this)))
  (epaint-put-image this :point point)
  (epaint-push-history this t))

(cl-defmethod epaint-put-image ((this epaint-canvas-class) &key point)
  (remove-images (point-min) (point-max))
  (setf (overlay this) (put-image (image this) (or point (point-min))))
  (image-flush (image this)))

(cl-defmethod epaint-clear-image ((this epaint-canvas-class) &optional need-query)
  (when (or (not need-query) (y-or-n-p "Really clear?"))
    (epaint-clear (drawable this) (background this))
    (image-flush (image this))
    (epaint-push-history this)))

(defun epaint-image-get-bitmap (image)
  (plist-get (cdr image) :data))

(cl-defmethod epaint-push-history ((this epaint-canvas-class) &optional clear)
  (with-slots (history) this
    (when clear
      (epaint-history-clear history))
    (epaint-history-fork-current history
                                 (copy-sequence (epaint-image-get-bitmap (image this))))
    (set-buffer-modified-p (not (epaint-history-checkpoint-p history)))))

(defun epaint-image-set-bitmap (image bitmap)
  (plist-put (cdr image) :data bitmap)
  (image-flush image)
  bitmap)

(cl-defmethod epaint-undo ((this epaint-canvas-class) &optional redo)
  (with-slots (drawable image history) this
    (when-let (bitmap (epaint-history-undo-current history redo))
      (setf (epaint--bitmap drawable)
            (epaint-image-set-bitmap image (copy-sequence bitmap))))
    (set-buffer-modified-p (not (epaint-history-checkpoint-p history)))))

(cl-defmethod epaint-repair ((this epaint-canvas-class))
  (epaint-bitmap-replace (epaint--bitmap (drawable this))
                         (epaint-history-current-data (history this)))
  (image-flush (image this)))

(cl-defmethod epaint-force-window-update ((this epaint-canvas-class) &optional redisplay)
  (image-flush (image this))
  (force-window-update (get-buffer-window))
  (when redisplay
    (redisplay)))

(cl-defmethod epaint-down-mouse-1 ((this epaint-canvas-class) ev)
  (let* ((history (history this))
         (drawable (drawable this))
         (gc (gc this))
         (image (image this))
         (position (event-start ev))
         (x-y (posn-object-x-y position))
         (x0 (car x-y))
         (y0 (cdr x-y))
         (sx x0)
         (sy y0)
         win obj x1 y1 x y clipped)
    ;; (setf (epaint-gc-current-x gc) sx
    ;;       (epaint-gc-current-y gc) sy)
    (track-mouse
      (while (or (mouse-movement-p ev)
                 (member 'down (event-modifiers ev)))
        (setq position (event-start ev)
              win (posn-window position)
              obj (posn-object position)
              x-y (posn-object-x-y position)
              x1 (car x-y)
              y1 (cdr x-y)
              x-y (posn-x-y position)
              x (car x-y)
              y (car x-y))

        (when (or (not (eq (get-buffer-window) win))
                  (not (eq image obj))
                  (zerop x)
                  (zerop y))
          (setq clipped t))

        (when (and (eq (get-buffer-window) win)
                   (eq image obj)
                   (/= 0 x)
                   (/= 0 y))
          (when clipped
            (setq x0 x1
                  y0 y1
                  clipped nil))

          (epaint-draw-funcall drawable gc
                               (epaint-history-current-data history)
                               sx sy x0 y0 x1 y1)
          (epaint-force-window-update this))

        (setq x0 x1
              y0 y1)
        (setq ev (read-event)))
      (epaint-push-history this)
      (setf (epaint-gc-current-x gc) x1
            (epaint-gc-current-y gc) y1))))

(cl-defmethod epaint--read-or-write-file-done ((this epaint-canvas-class) prefix filename)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) '(invisible t)))
  (set-visited-file-name filename)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (force-mode-line-update)
  (epaint-history-put-checkpoint (history this))
  (message (concat prefix " file done. [%s]") filename))

(cl-defmethod epaint-read-file ((this epaint-canvas-class) filename)
  (if-let (codec (epaint-plug-in-codec-get "read" filename))
      (cl-multiple-value-bind (read-func filename) codec
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents filename))
        (if (null (funcall read-func (drawable this)))
            (message "File read failure.")
          (epaint-create-image this))
        (epaint--read-or-write-file-done this "read" filename))
    (message "File read failure (unsupported format).")))

(cl-defmethod epaint-write-file ((this epaint-canvas-class) filename &optional ext)
  (when (eq epaint-internal-format 'xbm)
    (setf ext "xbm"))
  (if-let (codec (epaint-plug-in-codec-get "write" filename ext))
      (cl-multiple-value-bind (write-func filename) codec
        (with-temp-file filename
          (funcall write-func filename (drawable this)))
        (epaint--read-or-write-file-done this "write" filename))
    (message "Failure to file export (unsupported format).")))

(cl-defmethod epaint-write-back ((this epaint-canvas-class) filename &optional ext)
  (when (eq epaint-internal-format 'xbm)
    (setf ext "xbm"))
  (when-let (codec (epaint-plug-in-codec-get "write" filename ext))
    (cl-multiple-value-bind (write-func filename) codec
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall write-func filename (drawable this))
        (set-text-properties (point-min) (point-max) '(invisible t))
        (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defmethod epaint-change-shape ((this epaint-canvas-class) shape-symbol)
  (epaint-gc-change-shape (foreground this) shape-symbol)
  (epaint-gc-change-shape (background this) shape-symbol)
  (message "Changes the shape to %s." (symbol-name shape-symbol)))

(defconst epaint--rotate-colors [[255 64 64]
                                 [64 255 64]
                                 [64 64 255]])
(defvar epaint--rotate-color-index 0)

(cl-defmethod epaint-rotate-color ((this epaint-canvas-class))
  (let ((color (aref epaint--rotate-colors epaint--rotate-color-index)))
    (epaint-gc-set-color (foreground this) color)
    (setf epaint--rotate-color-index (% (cl-incf epaint--rotate-color-index)
                                        (length epaint--rotate-colors)))
    (message "Changes the color to %s." color)))

(cl-defmethod epaint-set-pen-type-erase ((this epaint-canvas-class))
  (setf (gc this) (if (eq (gc this) (foreground this))
                      (background this)
                    (foreground this)))
  (message "Changes the pen type to %s." (if (eq (gc this) (foreground this)) "pen" "eraser")))

(cl-defmethod epaint-set-pen-size ((this epaint-canvas-class) pen-size)
  (epaint-gc-set-pen-size (gc this) pen-size)
  (message "Change the pen size to %s." pen-size))

(cl-defmethod epaint-benchmarks ((this epaint-canvas-class))
  (cl-flet ((fill-test (drawable gc &optional loop-count)
                       (epaint-with-drawable (width height) drawable
                         (cl-loop for i below (or loop-count 100)
                                  if (cl-evenp i)
                                  do (epaint-with-timer "fill-test: epaint-clear"
                                       (epaint-clear drawable gc)
                                       (epaint-force-window-update this t))
                                  else
                                  do (epaint-with-timer "fill-test: epaint-set-pixel"
                                       (let (y x)
                                         (setq y 0)
                                         (while (< y height)
                                           (setq x 0)
                                           (while (< x width)
                                             (epaint-set-pixel drawable gc x y)
                                             (cl-incf x))
                                           (cl-incf y)))
                                       (epaint-force-window-update this t))))))
    (when (y-or-n-p "Run a benchmark?")
      (message "fill-test ---")
      (fill-test (drawable this) (gc this) 100)
      (epaint-repair this))))

(provide 'epaint-canvas)
;;; epaint-canvas.el ends here
