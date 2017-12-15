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

(require 'cl-lib)

;;; Helpers

(defmacro epaint-debug (level &rest body)
  (declare (indent 1))
  (when (and (boundp 'epaint-debug) epaint-debug)
    (if (eq (type-of epaint-debug) 'integer)
        (when (>= level epaint-debug)
          `(progn ,@body))
      (error "epaint-debug: The first argument is the specified level."))))

;;; History

(cl-defstruct (epaint-history-node (:conc-name epaint--))
  data prev next)

(defun epaint-history-node-to-string (node)
  (if (null node)
      "nil"
    (prin1-to-string (epaint--data node))))

(cl-defstruct (epaint-history (:conc-name epaint--))
  head current checkpoint)

(defun epaint-history-current-data (hist)
  (epaint--data (epaint--current hist)))

(defun epaint-history-clear (hist)
  (setf (epaint--head hist) nil
        (epaint--current hist) nil
        (epaint--checkpoint hist) nil))

(defun epaint-history-mapc (hist function)
  (let ((head (epaint--head hist)))
    (while head
      (funcall function head)
      (setf head (epaint--next head)))))

(defun epaint-history-mapcar (hist function)
  (let (ls)
    (epaint-history-mapc hist (lambda (node)
                                (push (funcall function node) ls)))
    (nreverse ls)))

(defun epaint-history-mapconcat (hist function separator)
  (mapconcat 'identity
             (epaint-history-mapcar hist function)
             separator))

(defun epaint-history-to-string (hist)
  (let (ls)
    (push "head" ls)
    (epaint-history-mapc hist
                         (lambda (node)
                           (push (epaint-history-node-to-string node) ls)))
    (push "nil" ls)
    (mapconcat 'identity (nreverse ls) " -> ")))

(defun epaint-history-print (hist)
  (message "%s, :current %s, :checkpoint %s"
           (epaint-history-to-string hist)
           (epaint-history-node-to-string (epaint--current hist))
           (epaint-history-node-to-string (epaint--checkpoint hist))))

(defun epaint-history-fork (hist origin-node data)
  (let ((node (make-epaint-history-node :data data
                                        :prev nil
                                        :next origin-node)))
    (when origin-node
      (setf (epaint--prev origin-node) node))
    (setf (epaint--head hist) node)))

(defun epaint-history-push (hist data)
  (epaint-history-fork hist (epaint--head hist) data))

(defun epaint-history-pop (hist)
  (let ((head (epaint--head hist)))
    (prog1 head
      (when (setf (epaint--head hist) (epaint--next head))
        (setf (epaint--prev head) nil)))))

(defun epaint-history-fork-current (hist data)
  (setf (epaint--current hist)
        (epaint-history-fork hist (epaint--current hist) data))
  (unless (epaint--checkpoint hist)
    (setf (epaint--checkpoint hist) (epaint--current hist))))

(defun epaint-history-undo-current (hist &optional redo)
  (let ((dest (if redo
                  (epaint--prev (epaint--current hist))
                (epaint--next (epaint--current hist)))))
    (if (null dest)
        (prog1 nil
          (message (concat "No further " (if redo "redo" "undo") " information")))
      (setf (epaint--current hist) dest)
      (epaint--data (epaint--current hist)))))

(defun epaint-history-put-checkpoint (hist)
  (setf (epaint--checkpoint hist) (epaint--current hist)))

(defun epaint-history-checkpoint-p (hist)
  (eq (epaint--checkpoint hist) (epaint--current hist)))

(epaint-debug 0
  (defun epaint-history-test ()
    (let ((hist (make-epaint-history))
          node0)
      (setf node0 (epaint-history-push hist 0))
      (cl-assert (string= "head -> 0 -> nil" (epaint-history-to-string hist)))
      (epaint-history-push hist 1)
      (cl-assert (string= "head -> 1 -> 0 -> nil" (epaint-history-to-string hist)))
      (epaint-history-push hist 2)
      (cl-assert (string= "head -> 2 -> 1 -> 0 -> nil" (epaint-history-to-string hist)))
      (epaint-history-pop hist)
      (cl-assert (string= "head -> 1 -> 0 -> nil" (epaint-history-to-string hist)))
      (epaint-history-pop hist)
      (cl-assert (string= "head -> 0 -> nil" (epaint-history-to-string hist)))
      (epaint-history-pop hist)
      (cl-assert (string= "head -> nil" (epaint-history-to-string hist)))
      (epaint-history-push hist 0)
      (epaint-history-push hist 1)
      (epaint-history-pop hist)
      (epaint-history-push hist 1)
      (cl-assert (string= "head -> 1 -> 0 -> nil" (epaint-history-to-string hist)))
      (epaint-history-push hist 2)
      (epaint-history-fork hist node0 3)
      (cl-assert (string= "head -> 3 -> 0 -> nil" (epaint-history-to-string hist)))
      "OK!")))

(provide 'epaint-history)
;;; epaint-history.el ends here
