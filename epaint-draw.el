;;; epaint-draw.el --- A single paint tool for Emacs  -*- lexical-binding: t; -*-

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

(require 'epaint-context)

;;; Helpers

(defsubst epaint-dist (x0 y0 x1 y1)
  (let ((dx (- x0 x1))
        (dy (- y0 y1)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defsubst epaint-clamp (x min max)
  (min (max x min) max))

(defsubst epaint-visible-p (width height x0 y0)
  (and (>= x0 0) (< x0 width) (>= y0 0) (< y0 height)))

(defsubst epaint-bitmap-replace (bitmap-dst bitmap-src)
  (let ((i 0)
        (len (length bitmap-src)))
    (while (< i len)
      (aset bitmap-dst i (aref bitmap-src i))
      (cl-incf i))))

;;; Drawing functions

(defsubst epaint-clear (drawable gc)
  (epaint-with-drawable (bitmap offset) drawable
    (epaint-with-gc (color) gc
      (epaint-bitmap-clear bitmap offset color))))

(defsubst epaint-get-pixel-linear (drawable i)
  (epaint-with-drawable (bitmap offset) drawable
    (when (< (+ offset i) (length bitmap))
      (epaint-bitmap-get-pixel-linear bitmap offset i))))

(defsubst epaint-set-pixel-linear (drawable gc i)
  (epaint-with-drawable (bitmap offset) drawable
    (epaint-with-gc (color) gc
      (when (< (+ offset i) (length bitmap))
        (epaint-bitmap-set-pixel-linear bitmap offset i color)))))

(defsubst epaint-get-pixel (drawable x0 y0)
  (epaint-with-drawable (bitmap offset width height) drawable
    (when (epaint-visible-p width height x0 y0)
      (epaint-bitmap-get-pixel-linear bitmap offset (+ (* y0 width) x0)))))

(defsubst epaint-set-pixel (drawable gc x0 y0)
  (epaint-with-drawable (bitmap offset width height) drawable
    (epaint-with-gc (color) gc
      (when (epaint-visible-p width height x0 y0)
        (epaint-bitmap-set-pixel-linear bitmap offset (+ (* y0 width) x0) color)))))

(defconst epaint--thick-point-coords
  [[0 0]
   [-1 -1 0 -1 1 -1 -1 0 0 0 1 0 -1 1 0 1 1 1]
   [-1 -2 0 -2 1 -2 -2 -1 -1 -1 0 -1 1 -1 2 -1 -2 0 -1 0 0 0 1 0 2 0 -2 1 -1 1 0 1 1 1 2 1 -1 2 0 2 1
       2]
   [-1 -3 0 -3 1 -3 -2 -2 -1 -2 0 -2 1 -2 2 -2 -3 -1 -2 -1 -1 -1 0 -1 1 -1 2 -1 3 -1 -3 0 -2 0 -1 0 0
       0 1 0 2 0 3 0 -3 1 -2 1 -1 1 0 1 1 1 2 1 3 1 -2 2 -1 2 0 2 1 2 2 2 -1 3 0 3 1 3]
   [-2 -4 -1 -4 0 -4 1 -4 2 -4 -3 -3 -2 -3 -1 -3 0 -3 1 -3 2 -3 3 -3 -4 -2 -3 -2 -2 -2 -1 -2 0 -2 1 -2
       2 -2 3 -2 4 -2 -4 -1 -3 -1 -2 -1 -1 -1 0 -1 1 -1 2 -1 3 -1 4 -1 -4 0 -3 0 -2 0 -1 0 0 0 1 0 2 0
       3 0 4 0 -4 1 -3 1 -2 1 -1 1 0 1 1 1 2 1 3 1 4 1 -4 2 -3 2 -2 2 -1 2 0 2 1 2 2 2 3 2 4 2 -3 3 -2
       3 -1 3 0 3 1 3 2 3 3 3 -2 4 -1 4 0 4 1 4 2 4]
   [-2 -5 -1 -5 0 -5 1 -5 2 -5 -3 -4 -2 -4 -1 -4 0 -4 1 -4 2 -4 3 -4 -4 -3 -3 -3 -2 -3 -1 -3 0 -3 1 -3
       2 -3 3 -3 4 -3 -5 -2 -4 -2 -3 -2 -2 -2 -1 -2 0 -2 1 -2 2 -2 3 -2 4 -2 5 -2 -5 -1 -4 -1 -3 -1 -2
       -1 -1 -1 0 -1 1 -1 2 -1 3 -1 4 -1 5 -1 -5 0 -4 0 -3 0 -2 0 -1 0 0 0 1 0 2 0 3 0 4 0 5 0 -5 1 -4
       1 -3 1 -2 1 -1 1 0 1 1 1 2 1 3 1 4 1 5 1 -5 2 -4 2 -3 2 -2 2 -1 2 0 2 1 2 2 2 3 2 4 2 5 2 -4 3
       -3 3 -2 3 -1 3 0 3 1 3 2 3 3 3 4 3 -3 4 -2 4 -1 4 0 4 1 4 2 4 3 4 -2 5 -1 5 0 5 1 5 2 5]
   [-3 -6 -2 -6 -1 -6 0 -6 1 -6 2 -6 3 -6 -4 -5 -3 -5 -2 -5 -1 -5 0 -5 1 -5 2 -5 3 -5 4 -5 -5 -4 -4 -4
       -3 -4 -2 -4 -1 -4 0 -4 1 -4 2 -4 3 -4 4 -4 5 -4 -6 -3 -5 -3 -4 -3 -3 -3 -2 -3 -1 -3 0 -3 1 -3 2
       -3 3 -3 4 -3 5 -3 6 -3 -6 -2 -5 -2 -4 -2 -3 -2 -2 -2 -1 -2 0 -2 1 -2 2 -2 3 -2 4 -2 5 -2 6 -2
       -6 -1 -5 -1 -4 -1 -3 -1 -2 -1 -1 -1 0 -1 1 -1 2 -1 3 -1 4 -1 5 -1 6 -1 -6 0 -5 0 -4 0 -3 0 -2 0
       -1 0 0 0 1 0 2 0 3 0 4 0 5 0 6 0 -6 1 -5 1 -4 1 -3 1 -2 1 -1 1 0 1 1 1 2 1 3 1 4 1 5 1 6 1 -6 2
       -5 2 -4 2 -3 2 -2 2 -1 2 0 2 1 2 2 2 3 2 4 2 5 2 6 2 -6 3 -5 3 -4 3 -3 3 -2 3 -1 3 0 3 1 3 2 3
       3 3 4 3 5 3 6 3 -5 4 -4 4 -3 4 -2 4 -1 4 0 4 1 4 2 4 3 4 4 4 5 4 -4 5 -3 5 -2 5 -1 5 0 5 1 5 2
       5 3 5 4 5 -3 6 -2 6 -1 6 0 6 1 6 2 6 3 6]
   [-2 -7 -1 -7 0 -7 1 -7 2 -7 -4 -6 -3 -6 -2 -6 -1 -6 0 -6 1 -6 2 -6 3 -6 4 -6 -5 -5 -4 -5 -3 -5 -2
       -5 -1 -5 0 -5 1 -5 2 -5 3 -5 4 -5 5 -5 -6 -4 -5 -4 -4 -4 -3 -4 -2 -4 -1 -4 0 -4 1 -4 2 -4 3 -4
       4 -4 5 -4 6 -4 -6 -3 -5 -3 -4 -3 -3 -3 -2 -3 -1 -3 0 -3 1 -3 2 -3 3 -3 4 -3 5 -3 6 -3 -7 -2 -6
       -2 -5 -2 -4 -2 -3 -2 -2 -2 -1 -2 0 -2 1 -2 2 -2 3 -2 4 -2 5 -2 6 -2 7 -2 -7 -1 -6 -1 -5 -1 -4
       -1 -3 -1 -2 -1 -1 -1 0 -1 1 -1 2 -1 3 -1 4 -1 5 -1 6 -1 7 -1 -7 0 -6 0 -5 0 -4 0 -3 0 -2 0 -1 0
       0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 -7 1 -6 1 -5 1 -4 1 -3 1 -2 1 -1 1 0 1 1 1 2 1 3 1 4 1 5 1 6 1
       7 1 -7 2 -6 2 -5 2 -4 2 -3 2 -2 2 -1 2 0 2 1 2 2 2 3 2 4 2 5 2 6 2 7 2 -6 3 -5 3 -4 3 -3 3 -2 3
       -1 3 0 3 1 3 2 3 3 3 4 3 5 3 6 3 -6 4 -5 4 -4 4 -3 4 -2 4 -1 4 0 4 1 4 2 4 3 4 4 4 5 4 6 4 -5 5
       -4 5 -3 5 -2 5 -1 5 0 5 1 5 2 5 3 5 4 5 5 5 -4 6 -3 6 -2 6 -1 6 0 6 1 6 2 6 3 6 4 6 -2 7 -1 7 0
       7 1 7 2 7]
   [-2 -8 -1 -8 0 -8 1 -8 2 -8 -4 -7 -3 -7 -2 -7 -1 -7 0 -7 1 -7 2 -7 3 -7 4 -7 -5 -6 -4 -6 -3 -6 -2
       -6 -1 -6 0 -6 1 -6 2 -6 3 -6 4 -6 5 -6 -6 -5 -5 -5 -4 -5 -3 -5 -2 -5 -1 -5 0 -5 1 -5 2 -5 3 -5
       4 -5 5 -5 6 -5 -7 -4 -6 -4 -5 -4 -4 -4 -3 -4 -2 -4 -1 -4 0 -4 1 -4 2 -4 3 -4 4 -4 5 -4 6 -4 7
       -4 -7 -3 -6 -3 -5 -3 -4 -3 -3 -3 -2 -3 -1 -3 0 -3 1 -3 2 -3 3 -3 4 -3 5 -3 6 -3 7 -3 -8 -2 -7
       -2 -6 -2 -5 -2 -4 -2 -3 -2 -2 -2 -1 -2 0 -2 1 -2 2 -2 3 -2 4 -2 5 -2 6 -2 7 -2 8 -2 -8 -1 -7 -1
       -6 -1 -5 -1 -4 -1 -3 -1 -2 -1 -1 -1 0 -1 1 -1 2 -1 3 -1 4 -1 5 -1 6 -1 7 -1 8 -1 -8 0 -7 0 -6 0
       -5 0 -4 0 -3 0 -2 0 -1 0 0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 -8 1 -7 1 -6 1 -5 1 -4 1 -3 1 -2 1
       -1 1 0 1 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 -8 2 -7 2 -6 2 -5 2 -4 2 -3 2 -2 2 -1 2 0 2 1 2 2 2 3
       2 4 2 5 2 6 2 7 2 8 2 -7 3 -6 3 -5 3 -4 3 -3 3 -2 3 -1 3 0 3 1 3 2 3 3 3 4 3 5 3 6 3 7 3 -7 4
       -6 4 -5 4 -4 4 -3 4 -2 4 -1 4 0 4 1 4 2 4 3 4 4 4 5 4 6 4 7 4 -6 5 -5 5 -4 5 -3 5 -2 5 -1 5 0 5
       1 5 2 5 3 5 4 5 5 5 6 5 -5 6 -4 6 -3 6 -2 6 -1 6 0 6 1 6 2 6 3 6 4 6 5 6 -4 7 -3 7 -2 7 -1 7 0
       7 1 7 2 7 3 7 4 7 -2 8 -1 8 0 8 1 8 2 8]
   [-2 -9 -1 -9 0 -9 1 -9 2 -9 -4 -8 -3 -8 -2 -8 -1 -8 0 -8 1 -8 2 -8 3 -8 4 -8 -6 -7 -5 -7 -4 -7 -3
       -7 -2 -7 -1 -7 0 -7 1 -7 2 -7 3 -7 4 -7 5 -7 6 -7 -7 -6 -6 -6 -5 -6 -4 -6 -3 -6 -2 -6 -1 -6 0
       -6 1 -6 2 -6 3 -6 4 -6 5 -6 6 -6 7 -6 -7 -5 -6 -5 -5 -5 -4 -5 -3 -5 -2 -5 -1 -5 0 -5 1 -5 2 -5
       3 -5 4 -5 5 -5 6 -5 7 -5 -8 -4 -7 -4 -6 -4 -5 -4 -4 -4 -3 -4 -2 -4 -1 -4 0 -4 1 -4 2 -4 3 -4 4
       -4 5 -4 6 -4 7 -4 8 -4 -8 -3 -7 -3 -6 -3 -5 -3 -4 -3 -3 -3 -2 -3 -1 -3 0 -3 1 -3 2 -3 3 -3 4 -3
       5 -3 6 -3 7 -3 8 -3 -9 -2 -8 -2 -7 -2 -6 -2 -5 -2 -4 -2 -3 -2 -2 -2 -1 -2 0 -2 1 -2 2 -2 3 -2 4
       -2 5 -2 6 -2 7 -2 8 -2 9 -2 -9 -1 -8 -1 -7 -1 -6 -1 -5 -1 -4 -1 -3 -1 -2 -1 -1 -1 0 -1 1 -1 2
       -1 3 -1 4 -1 5 -1 6 -1 7 -1 8 -1 9 -1 -9 0 -8 0 -7 0 -6 0 -5 0 -4 0 -3 0 -2 0 -1 0 0 0 1 0 2 0
       3 0 4 0 5 0 6 0 7 0 8 0 9 0 -9 1 -8 1 -7 1 -6 1 -5 1 -4 1 -3 1 -2 1 -1 1 0 1 1 1 2 1 3 1 4 1 5
       1 6 1 7 1 8 1 9 1 -9 2 -8 2 -7 2 -6 2 -5 2 -4 2 -3 2 -2 2 -1 2 0 2 1 2 2 2 3 2 4 2 5 2 6 2 7 2
       8 2 9 2 -8 3 -7 3 -6 3 -5 3 -4 3 -3 3 -2 3 -1 3 0 3 1 3 2 3 3 3 4 3 5 3 6 3 7 3 8 3 -8 4 -7 4
       -6 4 -5 4 -4 4 -3 4 -2 4 -1 4 0 4 1 4 2 4 3 4 4 4 5 4 6 4 7 4 8 4 -7 5 -6 5 -5 5 -4 5 -3 5 -2 5
       -1 5 0 5 1 5 2 5 3 5 4 5 5 5 6 5 7 5 -7 6 -6 6 -5 6 -4 6 -3 6 -2 6 -1 6 0 6 1 6 2 6 3 6 4 6 5 6
       6 6 7 6 -6 7 -5 7 -4 7 -3 7 -2 7 -1 7 0 7 1 7 2 7 3 7 4 7 5 7 6 7 -4 8 -3 8 -2 8 -1 8 0 8 1 8 2
       8 3 8 4 8 -2 9 -1 9 0 9 1 9 2 9]])

(defsubst epaint-draw-thick-point (drawable gc size x0 y0)
  (let* ((p (aref epaint--thick-point-coords (/ size 2)))
         (i 0)
         (len (length p)))
    (while (< i len)
      (epaint-set-pixel drawable gc (+ x0 (aref p i)) (+ y0 (aref p (1+ i))))
      (cl-incf i 2))))

(defun epaint-draw-line (drawable gc x0 y0 x1 y1)
  "http://members.chello.at/~easyfilter/bresenham.html"
  (let* ((dx (abs (- x1 x0)))
         (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1))
         (sy (if (< y0 y1) 1 -1))
         (err (+ dx dy))
         e2 finish)
    (while (not finish)
      (epaint-set-pixel drawable gc x0 y0)
      (if (and (= x0 x1) (= y0 y1))
          (setq finish t)
        (setq e2 (* 2 err))
        (when (>= e2 dy)
          (setq err (+ err dy)
                x0 (+ x0 sx)))
        (when (<= e2 dx)
          (setq err (+ err dx)
                y0 (+ y0 sy)))))))

;; Drawing Thick Lines
;;   Murphy's Modified Bresenham Line Drawing Algorithm
;;         http://www.zoo.co.uk/murphy/thickline/
;;   Reference from http://kt8216.unixcab.org/murphy/index.html

;;***********************************************************************
;;*                                                                     *
;;*                            X BASED LINES                            *
;;*                                                                     *
;;***********************************************************************

(defsubst epaint--x-perpendicular (drawable gc x0 y0 dx dy xstep ystep
                                            einit w_left w_right winit)
  (let (x y threshold E_diag E_square tk error p q)
    (setq threshold (- dx (* 2 dy))
          E_diag (* -2 dx)
          E_square (* 2 dy)
          p 0
          q 0

          y y0
          x x0
          error einit
          tk (- (+ dx dy) winit))

    (while (<= tk w_left)
      (epaint-set-pixel drawable gc x y)
      (when (>= error threshold)
        (setq x (+ x xstep)
              error (+ error E_diag)
              tk (+ tk (* 2 dy))))
      (setq error (+ error E_square)
            y (+ y ystep)
            tk (+ tk (* 2 dx))
            q (1+ q)))

    (setq y y0
          x x0
          error (- einit)
          tk (+ dx dy winit))

    (while (<= tk w_right)
      (when (/= 0 p)
        (epaint-set-pixel drawable gc x y))
      (when (> error threshold)
        (setq x (- x xstep)
              error (+ error E_diag)
              tk (+ tk (* 2 dy))))
      (setq error (+ error E_square)
            y (- y ystep)
            tk (+ tk (* 2 dx))
            p (1+ p)))

    (when (and (zerop q) (< p 2))
      (epaint-set-pixel drawable gc x0 y0)))) ; we need this for very thin lines

(defun epaint--x-varthick-line (drawable gc x0 y0 dx dy xstep ystep
                                         left argL
                                         right argR pxstep pystep)
  (let (p_error error x y threshold E_diag E_square length w_left w_right D)
    (setq p_error 0
          error 0
          y y0
          x x0
          threshold (- dx (* 2 dy))
          E_diag (* -2 dx)
          E_square (* 2 dy)
          length (1+ dx)
          D (sqrt (+ (* dx dx) (* dy dy))))

    (cl-loop for p below length
             do (setq w_left (* (funcall left argL p length) 2 D)
                      w_right (* (funcall right argR p length) 2 D))
             (epaint--x-perpendicular drawable gc x y dx dy pxstep pystep
                                      p_error w_left w_right error)

             (when (>= error threshold)
               (setq y (+ y ystep)
                     error (+ error E_diag))
               (when (>= p_error threshold)
                 (epaint--x-perpendicular drawable gc x y dx dy pxstep pystep
                                          (+ p_error E_diag E_square) w_left w_right error)
                 (setq p_error (+ p_error E_diag)))
               (setq p_error (+ p_error E_square)))
             (setq error (+ error E_square)
                   x (+ x xstep)))))

(defun epaint--x-thick-line (drawable gc size x0 y0 dx dy xstep ystep
                                      pxstep pystep)
  (let (p_error error x y threshold E_diag E_square length w_left w_right D)
    (setq p_error 0
          error 0
          y y0
          x x0
          threshold (- dx (* 2 dy))
          E_diag (* -2 dx)
          E_square (* 2 dy)
          length (1+ dx)
          D (sqrt (+ (* dx dx) (* dy dy))))

    (cl-loop for p below length
             do (setq w_left (* size 2 D)
                      w_right (* size 2 D))
             (epaint--x-perpendicular drawable gc x y dx dy pxstep pystep
                                      p_error w_left w_right error)

             (when (>= error threshold)
               (setq y (+ y ystep)
                     error (+ error E_diag))
               (when (>= p_error threshold)
                 (epaint--x-perpendicular drawable gc x y dx dy pxstep pystep
                                          (+ p_error E_diag E_square) w_left w_right error)
                 (setq p_error (+ p_error E_diag)))
               (setq p_error (+ p_error E_square)))
             (setq error (+ error E_square)
                   x (+ x xstep)))))

;;***********************************************************************
;;*                                                                     *
;;*                            Y BASED LINES                            *
;;*                                                                     *
;;***********************************************************************

(defsubst epaint--y-perpendicular (drawable gc x0 y0 dx dy xstep ystep
                                            einit w_left w_right winit)
  (let (x y threshold E_diag E_square tk error p q)
    (setq p 0
          q 0
          threshold (- dy (* 2 dx))
          E_diag (* -2 dy)
          E_square (* 2 dx)

          y y0
          x x0
          error (- einit)
          tk (+ dx dy winit))

    (while (<= tk w_left)
      (epaint-set-pixel drawable gc x y)
      (when (> error threshold)
        (setq y (+ y ystep)
              error (+ error E_diag)
              tk (+ tk (* 2 dx))))
      (setq error (+ error E_square)
            x (+ x xstep)
            tk (+ tk (* 2 dy))
            q (1+ q)))

    (setq y y0
          x x0
          error einit
          tk (- (+ dx dy) winit))

    (while (<= tk w_right)
      (when (/= 0 p)
        (epaint-set-pixel drawable gc x y))
      (when (>= error threshold)
        (setq y (- y ystep)
              error (+ error E_diag)
              tk (+ tk (* 2 dx))))
      (setq error (+ error E_square)
            x (- x xstep)
            tk (+ tk (* 2 dy))
            p (1+ p)))

    (when (and (zerop q) (< p 2))
      (epaint-set-pixel drawable gc x0 y0)))) ; we need this for very thin lines

(defun epaint--y-varthick-line (drawable gc x0 y0 dx dy xstep ystep
                                         left argL
                                         right argR pxstep pystep)
  (let (p_error error x y threshold E_diag E_square length w_left w_right D)
    (setq p_error 0
          error 0
          y y0
          x x0
          threshold (- dy (* 2 dx))
          E_diag (* -2 dy)
          E_square (* 2 dx)
          length (1+ dy)
          D (sqrt (+ (* dx dx) (* dy dy))))

    (cl-loop for p below length
             do (setq w_left (* (funcall left argL p length) 2 D)
                      w_right (* (funcall right argR p length) 2 D))
             (epaint--y-perpendicular drawable gc x y dx dy pxstep pystep
                                      p_error w_left w_right error)

             (when (>= error threshold)
               (setq x (+ x xstep)
                     error (+ error E_diag))
               (when (>= p_error threshold)
                 (epaint--y-perpendicular drawable gc x y dx dy pxstep pystep
                                          (+ p_error E_diag E_square) w_left w_right error)
                 (setq p_error (+ p_error E_diag)))
               (setq p_error (+ p_error E_square)))
             (setq error (+ error E_square)
                   y (+ y ystep)))))

(defun epaint--y-thick-line (drawable gc size x0 y0 dx dy xstep ystep
                                      pxstep pystep)
  (let (p_error error x y threshold E_diag E_square length w_left w_right D)
    (setq p_error 0
          error 0
          y y0
          x x0
          threshold (- dy (* 2 dx))
          E_diag (* -2 dy)
          E_square (* 2 dx)
          length (1+ dy)
          D (sqrt (+ (* dx dx) (* dy dy))))

    (cl-loop for p below length
             do (setq w_left (* size 2 D)
                      w_right (* size 2 D))
             (epaint--y-perpendicular drawable gc x y dx dy pxstep pystep
                                      p_error w_left w_right error)

             (when (>= error threshold)
               (setq x (+ x xstep)
                     error (+ error E_diag))
               (when (>= p_error threshold)
                 (epaint--y-perpendicular drawable gc x y dx dy pxstep pystep
                                          (+ p_error E_diag E_square) w_left w_right error)
                 (setq p_error (+ p_error E_diag)))
               (setq p_error (+ p_error E_square)))
             (setq error (+ error E_square)
                   y (+ y ystep)))))

;;***********************************************************************
;;*                                                                     *
;;*                                ENTRY                                *
;;*                                                                     *
;;***********************************************************************

(defun epaint--draw-varthick-line (drawable gc x0 y0 x1 y1 left argL right argR)
  (let (dx dy xstep ystep pxstep pystep xch)
    (setq dx (- x1 x0)
          dy (- y1 y0)
          xstep 1
          ystep 1)

    (when (< dx 0) (setq dx (- dx) xstep -1))
    (when (< dy 0) (setq dy (- dy) ystep -1))

    (when (zerop dx) (setq xstep 0))
    (when (zerop dy) (setq ystep 0))

    (setq xch 0)
    (let ((d (+ xstep (* ystep 4))))
      (cond ((= d -5) (setq pystep -1 pxstep  1 xch 1))	  ; -1 + -1*4
            ((= d -1) (setq pystep -1 pxstep  0 xch 1))	  ; -1 +  0*4
            ((= d  3) (setq pystep  1 pxstep  1))	  ; -1 +  1*4
            ((= d -4) (setq pystep  0 pxstep -1))	  ;  0 + -1*4
            ((= d  0) (setq pystep  0 pxstep  0))	  ;  0 +  0*4
            ((= d  4) (setq pystep  0 pxstep  1))	  ;  0 +  1*4
            ((= d -3) (setq pystep -1 pxstep -1))	  ;  1 + -1*4
            ((= d  1) (setq pystep -1 pxstep  0))	  ;  1 +  0*4
            ((= d  5) (setq pystep  1 pxstep -1 xch 1)))) ;  1 +  1*4

    (when (/= 0 xch)
      (let (K)
        (setq K argL argL argR argR K
              K left left right right K)))

    (if (> dx dy)
        (epaint--x-varthick-line drawable gc x0 y0 dx dy xstep ystep
                                 left argL right argR
                                 pxstep pystep)
      (epaint--y-varthick-line drawable gc x0 y0 dx dy xstep ystep
                               left argL right argR
                               pxstep pystep))))

(defun epaint--draw-thick-line (drawable gc size x0 y0 x1 y1)
  (let (dx dy xstep ystep pxstep pystep)
    (setq dx (- x1 x0)
          dy (- y1 y0)
          xstep 1
          ystep 1)

    (when (< dx 0) (setq dx (- dx) xstep -1))
    (when (< dy 0) (setq dy (- dy) ystep -1))

    (when (zerop dx) (setq xstep 0))
    (when (zerop dy) (setq ystep 0))

    (let ((d (+ xstep (* ystep 4))))
      (cond ((= d -5) (setq pystep -1 pxstep  1))   ; -1 + -1*4
            ((= d -1) (setq pystep -1 pxstep  0))   ; -1 +  0*4
            ((= d  3) (setq pystep  1 pxstep  1))   ; -1 +  1*4
            ((= d -4) (setq pystep  0 pxstep -1))   ;  0 + -1*4
            ((= d  0) (setq pystep  0 pxstep  0))   ;  0 +  0*4
            ((= d  4) (setq pystep  0 pxstep  1))   ;  0 +  1*4
            ((= d -3) (setq pystep -1 pxstep -1))   ;  1 + -1*4
            ((= d  1) (setq pystep -1 pxstep  0))   ;  1 +  0*4
            ((= d  5) (setq pystep  1 pxstep -1)))) ;  1 +  1*4

    (if (> dx dy)
        (epaint--x-thick-line drawable gc size x0 y0 dx dy xstep ystep pxstep pystep)
      (epaint--y-thick-line drawable gc size x0 y0 dx dy xstep ystep pxstep pystep))))

(defsubst epaint-draw-thick-line (drawable gc size x0 y0 x1 y1)
  (when (or (/= x0 x1) (/= y0 y1))
    (epaint--draw-thick-line drawable gc (/ (1+ size) 2) x0 y0 x1 y1)))

(defun epaint-draw-circle-with-radius (drawable gc radius x0 y0)
  "http://members.chello.at/~easyfilter/bresenham.html"
  (let ((x (- radius))
        (y 0)
        (err (- 2 (* 2 radius))))
    (cl-loop do
             (epaint-set-pixel drawable gc (- x0 x) (+ y0 y))
             (epaint-set-pixel drawable gc (- x0 y) (- y0 x))
             (epaint-set-pixel drawable gc (+ x0 x) (- y0 y))
             (epaint-set-pixel drawable gc (+ x0 y) (+ y0 x))
             (setq radius err)
             (when (<= radius y)
               (setq err (+ err (1+ (* (cl-incf y) 2)))))
             (when (or (> radius x) (> err y))
               (setq err (+ err (1+ (* (cl-incf x) 2)))))
             while (< x 0))))

(defsubst epaint-draw-circle (drawable gc x0 y0 x1 y1)
  (epaint-draw-circle-with-radius drawable gc
                                  (truncate (epaint-dist x0 y0 x1 y1))
                                  x0 y0))

(defsubst epaint--x-line (drawable gc x0 x1 y)
  (while (<= x0 x1)
    (epaint-set-pixel drawable gc x0 y)
    (cl-incf x0)))

(defsubst epaint--y-line (drawable gc x y0 y1)
  (while (<= y0 y1)
    (epaint-set-pixel drawable gc x y0)
    (cl-incf y0)))

(defun epaint-draw-thick-circle-with-radius (drawable gc inner outer x0 y0)
  (let ((xo (- outer))
        (xi (- inner))
        (yo 0)
        (yi 0)
        (erro (- 2 (* 2 outer)))
        (erri (- 2 (* 2 inner))))
    (cl-loop do
             (epaint--x-line drawable gc (- x0 xi) (- x0 xo) (+ y0 yo))
             (epaint--x-line drawable gc (- x0 xi) (- x0 xo) (- y0 yo))
             (epaint--y-line drawable gc (- x0 yo) (- y0 xi) (- y0 xo))
             (epaint--y-line drawable gc (+ x0 yo) (- y0 xi) (- y0 xo))
             (epaint--x-line drawable gc (+ x0 xo) (+ x0 xi) (- y0 yo))
             (epaint--x-line drawable gc (+ x0 xo) (+ x0 xi) (+ y0 yo))
             (epaint--y-line drawable gc (+ x0 yo) (+ y0 xo) (+ y0 xi))
             (epaint--y-line drawable gc (- x0 yo) (+ y0 xo) (+ y0 xi))

             (setq outer erro)
             (setq inner erri)

             (when (<= outer yo)
               (setq erro (+ erro (1+ (* (cl-incf yo) 2)))))
             (when (<= inner yi)
               (setq erri (+ erri (1+ (* (cl-incf yi) 2)))))

             (when (or (> outer xo) (> erro yo))
               (setq erro (+ erro (1+ (* (cl-incf xo) 2)))))
             (when (and (< xi 0) (or (> inner xi) (> erri yi)))
               (setq erri (+ erri (1+ (* (cl-incf xi) 2)))))

             while (< xo 0))))

(defsubst epaint-draw-thick-circle (drawable gc size x0 y0 x1 y1)
  (let* ((outer (truncate (epaint-dist x0 y0 x1 y1)))
         (inner (max 0 (1+ (- outer size)))))
    (epaint-draw-thick-circle-with-radius drawable gc inner outer x0 y0)))

(defun epaint-draw-ellipse (drawable gc x0 y0 x1 y1)
  "http://members.chello.at/~easyfilter/bresenham.html"
  (let* ((a (abs (- x1 x0)))
         (b (abs (- y1 y0)))
         (b1 (logand b 1))
         (dx (* 4 (- 1 a) b b))
         (dy (* 4 (+ b1 1) a a))
         (err (+ dx dy (* b1 a a)))
         e2)
    (when (> x0 x1)
      (setq x0 x1
            x1 (+ x1 a)))
    (when (> y0 y1)
      (setq y0 y1))
    (setq y0 (+ y0 (/ (+ b 1) 2))
          y1 (- y0 b1)
          a (* 8 a a)
          b1 (* 8 b b))
    (cl-loop do
             (epaint-set-pixel drawable gc x1 y0)
             (epaint-set-pixel drawable gc x0 y0)
             (epaint-set-pixel drawable gc x0 y1)
             (epaint-set-pixel drawable gc x1 y1)
             (setq e2 (* 2 err))
             (when (<= e2 dy)
               (cl-incf y0)
               (cl-decf y1)
               (setq dy (+ dy a)
                     err (+ err dy)))
             (when (or (>= e2 dx) (> (* 2 err) dy))
               (cl-incf x0)
               (cl-decf x1)
               (setq dx (+ dx b1)
                     err (+ err dx)))
             while (<= x0 x1))
    (while (< (- y0 y1) b)
      (epaint-set-pixel drawable gc (- x0 1) y0)
      (epaint-set-pixel drawable gc (+ x1 1) y0) (cl-incf y0)
      (epaint-set-pixel drawable gc (- x0 1) y1)
      (epaint-set-pixel drawable gc (+ x1 1) y1) (cl-decf y1))))

(defun epaint-draw-thick-ellipse (drawable gc size x0 y0 x1 y1)
  (setq size (* (1- size) 2))
  (when (> x0 x1)
    (cl-rotatef x0 x1))
  (when (> y0 y1)
    (cl-rotatef y0 y1))

  (let* ((x0o x0)
         (x1o x1)
         (y0o y0)
         (y1o y1)

         (ao (- x1o x0o))
         (bo (- y1o y0o))
         (b1o (logand bo 1))
         (dxo (* 4 (- 1 ao) bo bo))
         (dyo (* 4 (+ b1o 1) ao ao))
         (erro (+ dxo dyo (* b1o ao ao)))
         e2o

         (a (/ (min ao size) 2))
         (b (/ (min bo size) 2))
         (x0i (+ x0 a))
         (x1i (- x1 a))
         (y0i (+ y0 b))
         (y1i (- y1 b))

         (ai (- x1i x0i))
         (bi (- y1i y0i))
         (b1i (logand bi 1))
         (dxi (* 4 (- 1 ai) bi bi))
         (dyi (* 4 (+ b1i 1) ai ai))
         (erri (+ dxi dyi (* b1i ai ai)))
         e2i)

    (setq y0o (+ y0o (/ (+ bo 1) 2))
          y1o (- y0o b1o)
          ao (* 8 ao ao)
          b1o (* 8 bo bo))

    (setq y0i (+ y0i (/ (+ bi 1) 2))
          y1i (- y0i b1i)
          ai (* 8 ai ai)
          b1i (* 8 bi bi))

    (cl-loop do
             (epaint--x-line drawable gc x1i x1o y0i)
             (epaint--x-line drawable gc x0o x0i y0i)
             (epaint--x-line drawable gc x1i x1o y1i)
             (epaint--x-line drawable gc x0o x0i y1i)
             (epaint--y-line drawable gc x0o y0i y0o)
             (epaint--y-line drawable gc x1o y0i y0o)
             (epaint--y-line drawable gc x0o y1o y1i)
             (epaint--y-line drawable gc x1o y1o y1i)

             (setq e2o (* 2 erro))
             (when (<= e2o dyo)
               (cl-incf y0o)
               (cl-decf y1o)
               (setq dyo (+ dyo ao)
                     erro (+ erro dyo)))
             (when (or (>= e2o dxo) (> (* 2 erro) dyo))
               (cl-incf x0o)
               (cl-decf x1o)
               (setq dxo (+ dxo b1o)
                     erro (+ erro dxo)))

             (when (<= x0i x1i)
               (setq e2i (* 2 erri))
               (when (<= e2i dyi)
                 (cl-incf y0i)
                 (cl-decf y1i)
                 (setq dyi (+ dyi ai)
                       erri (+ erri dyi)))
               (when (or (>= e2i dxi) (> (* 2 erri) dyi))
                 (cl-incf x0i)
                 (cl-decf x1i)
                 (setq dxi (+ dxi b1i)
                       erri (+ erri dxi))))

             while (<= x0o x1o))

    (while (< (- y0o y1o) bo)
      (epaint-set-pixel drawable gc (- x0o 1) y0o)
      (epaint-set-pixel drawable gc (+ x1o 1) y0o) (cl-incf y0o)
      (epaint-set-pixel drawable gc (- x0o 1) y1o)
      (epaint-set-pixel drawable gc (+ x1o 1) y1o) (cl-decf y1o))))

(defun epaint-draw-rectangle (drawable gc x0 y0 x1 y1)
  (when (> x0 x1)
    (cl-rotatef x0 x1))
  (when (> y0 y1)
    (cl-rotatef y0 y1))
  (epaint--x-line drawable gc x0 x1 y0)
  (epaint--x-line drawable gc x0 x1 y1)
  (epaint--y-line drawable gc x0 y0 y1)
  (epaint--y-line drawable gc x1 y0 y1))

(defsubst epaint--fill-rectangle (drawable gc x0 y0 x1 y1)
  (while (<= y0 y1)
    (epaint--x-line drawable gc x0 x1 y0)
    (cl-incf y0)))

(defun epaint-draw-fill-rectangle (drawable gc x0 y0 x1 y1)
  (when (> x0 x1)
    (cl-rotatef x0 x1))
  (when (> y0 y1)
    (cl-rotatef y0 y1))
  (epaint--fill-rectangle drawable gc x0 y0 x1 y1))

(defun epaint-draw-thick-rectangle (drawable gc size x0 y0 x1 y1)
  (cl-decf size)
  (when (> x0 x1)
    (cl-rotatef x0 x1))
  (when (> y0 y1)
    (cl-rotatef y0 y1))
  (let ((xsize (min (- x1 x0) size))
        (ysize (min (- y1 y0) size)))
    (epaint--fill-rectangle drawable gc x0 y0 x1 (+ y0 ysize))
    (epaint--fill-rectangle drawable gc x0 (- y1 ysize) x1 y1)
    (epaint--fill-rectangle drawable gc x0 (+ y0 ysize) (+ x0 xsize) (- y1 ysize))
    (epaint--fill-rectangle drawable gc (- x1 xsize) (+ y0 ysize) x1 (- y1 ysize))))

(defun epaint--draw-fill-triangle (drawable gc x0 y0 x1 y1 x2 y2)
  "http://www.sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html

This method rasterizes a triangle using only integer variables by using a
modified bresenham algorithm.
It's important that (x1, y1) and (x2, y2) lie on the same horizontal line, that is
y1 must be equal to y2."
  (let ((tx0 x0)
        (ty0 y0)
        (tx1 x0)
        (ty1 y0)

        (changed1 nil)
        (changed2 nil)

        (dx1 (abs (- x1 x0)))
        (dy1 (abs (- y1 y0)))

        (dx2 (abs (- x2 x0)))
        (dy2 (abs (- y2 y0)))

        (signx1 (cl-signum (- x1 x0)))
        (signx2 (cl-signum (- x2 x0)))

        (signy1 (cl-signum (- y1 y0)))
        (signy2 (cl-signum (- y2 y0)))
        e1 e2 (i 0))

    (when (> dy1 dx1)
      (cl-rotatef dx1 dy1)
      (setq changed1 t))

    (when (> dy2 dx2)
      (cl-rotatef dx2 dy2)
      (setq changed2 t))

    (setq e1 (- (* 2 dy1) dx1)
          e2 (- (* 2 dy2) dx2))

    (while (<= i dx1)
      (epaint-draw-line drawable gc tx0 ty0 tx1 ty1)

      (while (>= e1 0)
        (if changed1
            (setq tx0 (+ tx0 signx1))
          (setq ty0 (+ ty0 signy1)))
        (setq e1 (- e1 (* 2 dx1))))

      (if changed1
          (setq ty0 (+ ty0 signy1))
        (setq tx0 (+ tx0 signx1)))

      (setq e1 (+ e1 (* 2 dy1)))

      ;; here we rendered the next point on line 1 so follow now line 2
      ;; until we are on the same y-value as line 1.
      (while (/= ty1 ty0)
        (while (>= e2 0)
          (if changed2
              (setq tx1 (+ tx1 signx2))
            (setq ty1 (+ ty1 signy2)))
          (setq e2 (- e2 (* 2 dx2))))

        (if changed2
            (setq ty1 (+ ty1 signy2))
          (setq tx1 (+ tx1 signx2)))

        (setq e2 (+ e2 (* 2 dy2))))

      (cl-incf i))))

(defun epaint-draw-fill-triangle (drawable gc x0 y0 x1 y1 x2 y2)
  ;; at first sort the three vertices by y-coordinate ascending,
  ;; so (x0, y0) is the topmost vertice
  (when (> y0 y1)
    (cl-rotatef x0 x1)
    (cl-rotatef y0 y1))
  (when (> y0 y2)
    (cl-rotatef x0 x2)
    (cl-rotatef y0 y2))
  (when (> y1 y2)
    (cl-rotatef x1 x2)
    (cl-rotatef y1 y2))
  ;; here we know that y0 <= y1 <= y2
  ;; check for trivial case of bottom-flat triangle
  (if (= y1 y2)
      (epaint--draw-fill-triangle drawable gc x0 y0 x1 y1 x2 y2)
    ;; check for trivial case of top-flat triangle
    (if (= y0 y1)
        (epaint--draw-fill-triangle drawable gc x2 y2 x0 y0 x1 y1)
      ;; general case - split the triangle in a topflat and bottom-flat one
      (let ((tx (truncate (+ x0 (* (/ (float (- y1 y0)) (float (- y2 y0))) (- x2 x0)))))
            (ty y1))
        (epaint--draw-fill-triangle drawable gc x0 y0 x1 y1 tx ty)
        (epaint--draw-fill-triangle drawable gc x2 y2 x1 y1 tx ty)))))

(defun epaint-draw-isosceles-triangle (drawable gc x0 y0 x1 y1)
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (x2 (+ x0 dy))
         (y2 (+ y0 (- dx)))
         (x3 (+ x0 (- dy)))
         (y3 (+ y0 dx)))
    (when (or (/= dx 0) (/= dy 0))
      (epaint-draw-fill-triangle drawable gc x1 y1 x2 y2 x3 y3))))

(defun epaint--draw-heart-left-right (_arg point length)
  (let* ((c (* (abs (/ (float point) length)) 10.0)))
    (- c (- 4 (* c c)))))

(defun epaint-draw-heart (drawable gc x0 y0 x1 y1)
  (when (or (/= x0 x1) (/= y0 y1))
    (epaint--draw-varthick-line drawable gc x0 y0 x1 y1
                                'epaint--draw-heart-left-right nil
                                'epaint--draw-heart-left-right nil)))

;;; Draw Function Table

(defvar epaint-draw-function-table nil)

(defun epaint-draw-function-add (symbol-shape function &optional need-restore need-connect)
  (cl-pushnew (list symbol-shape function need-restore need-connect)
              epaint-draw-function-table :test 'equal))

(defsubst epaint-draw-funcall (drawable gc prev-bitmap sx sy x0 y0 x1 y1)
  (epaint-with-drawable (bitmap) drawable
    (epaint-with-gc (size current-x current-y draw-func restore-p connect-p) gc
      (when draw-func
        (when restore-p
          (epaint-bitmap-replace bitmap prev-bitmap))
        (funcall draw-func drawable gc size
                 (if connect-p current-x sx) (if connect-p current-y sy) x0 y0 x1 y1)))))

(defun epaint-draw-function-freehand (drawable gc size _sx _sy x0 y0 x1 y1)
  (if (= size 1)
      (epaint-draw-line drawable gc x0 y0 x1 y1)
    (epaint-draw-thick-line drawable gc size x0 y0 x1 y1)
    (epaint-draw-thick-point drawable gc size x1 y1)))

(defun epaint-draw-function-circle (drawable gc size sx sy _x0 _y0 x1 y1)
  (if (= size 1)
      (epaint-draw-circle drawable gc sx sy x1 y1)
    (epaint-draw-thick-circle drawable gc size sx sy x1 y1)))

(defun epaint-draw-function-ellipse (drawable gc size sx sy _x0 _y0 x1 y1)
  (if (= size 1)
      (epaint-draw-ellipse drawable gc sx sy x1 y1)
    (epaint-draw-thick-ellipse drawable gc size sx sy x1 y1)))

(defun epaint-draw-function-rectangle (drawable gc size sx sy _x0 _y0 x1 y1)
  (if (= size 1)
      (epaint-draw-rectangle drawable gc sx sy x1 y1)
    (epaint-draw-thick-rectangle drawable gc size sx sy x1 y1)))

(defun epaint-draw-function-line (drawable gc size sx sy _x0 _y0 x1 y1)
  (if (= size 1)
      (epaint-draw-line drawable gc sx sy x1 y1)
    (epaint-draw-thick-line drawable gc size sx sy x1 y1)))

(defun epaint-draw-function-arrow (drawable gc size sx sy _x0 _y0 x1 y1)
  (let* ((sz (max 7 size))
         (dx (- x1 sx))
         (dy (- y1 sy))
         (dist (sqrt (+ (* dx dx) (* dy dy))))
         (nx (* (/ dx dist) sz 1.5))
         (ny (* (/ dy dist) sz 1.5))
         (mx (- x1 (truncate nx)))
         (my (- y1 (truncate ny)))
         (mx2 (- x1 (truncate (/ nx 2))))
         (my2 (- y1 (truncate (/ ny 2)))))
    (epaint-draw-isosceles-triangle drawable gc mx my x1 y1)
    (when (> dist size)
      (if (= size 1)
          (epaint-draw-line drawable gc sx sy mx2 my2)
        (epaint-draw-thick-line drawable gc size sx sy mx2 my2)))))

(provide 'epaint-draw)
;;; epaint-draw.el ends here

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
