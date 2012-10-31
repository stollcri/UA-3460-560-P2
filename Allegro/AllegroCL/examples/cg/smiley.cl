;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2007 Franz Inc, Oakland, CA  All rights reserved.
;;
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-

;; An amusing demonstration of the Runtime System.
;; (A standalone application can be created with run-smiley-example as the
;; entry function.)

(in-package :cg-user)

(defconstant smiley-radius 50)
(defconstant smiley-centre
    (make-position (1+ smiley-radius) (1+ smiley-radius)))
(defconstant smiley-box-side (+ (* smiley-radius 2) 3))

(defun smiley-bitmap-stream ()
   (let ((bw (open-stream 'bitmap-stream nil nil
               :page-width smiley-box-side
               :page-height smiley-box-side))
         (left-eye (position+ smiley-centre (make-position -21 -11)))
         (right-eye (position+ smiley-centre (make-position 21 -11)))
         (left-pupil (position+ smiley-centre (make-position -21 -6)))
         (right-pupil (position+ smiley-centre (make-position 21 -6))))
      (setf (foreground-color bw) magenta)
      (fill-circle bw smiley-centre smiley-radius)
      (erase-circle-arc bw smiley-centre 35 30 120)
      (setf (foreground-color bw) white)
      (fill-circle bw left-eye 10)
      (fill-circle bw right-eye 10)
      (setf (foreground-color bw) blue)
      (fill-circle bw left-pupil 5)
      (fill-circle bw right-pupil 5)
      bw))

(defun next-centre (centre smiley-stream)
  (with-boxes (box1)
    (let* ((step #.(make-position 1 1))
           (bounding-box (nvisible-box smiley-stream box1))
           (vertical-bounce?
            (cond
             ((minusp (box-top centre))
              (setq step (nmake-position step (position-x step) 1)))
             ((> (box-bottom centre) (box-bottom bounding-box))
              (setq step (nmake-position step (position-x step) -1)))))
           (horizontal-bounce?
            (cond
             ((minusp (box-left centre))
              (setq step (nmake-position step 1 (position-y step))))
             ((> (box-right centre) (box-right bounding-box))
              (setq step (nmake-position step -1 (position-y step)))))))
      (when (or horizontal-bounce? vertical-bounce?) 
        #+aauugghh!
        (beep smiley-stream))
      (nbox-move centre step))))
  
;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
(defun run-smiley-example ()
  (with-open-stream
      (smiley-stream (make-window :bitmap-window
                       :class 'bitmap-window
                       :owner (screen *system*)
                       :title "Smiley" 
                       :scrollbars nil))
    (do  ((to-box (make-box-relative 10 10 smiley-box-side smiley-box-side))
          (smiley-bitmap-stream  (smiley-bitmap-stream))
          (smiley-box (make-box 0 0 smiley-box-side smiley-box-side))
          )
        ((not (windowp smiley-stream)))
      (copy-stream-area (frame-child smiley-stream) smiley-bitmap-stream 
                        (next-centre to-box smiley-stream)
                        smiley-box
                        po-replace)
      (process-pending-events))
    
    ;; If the window has been closed and we're running in standalone
    ;; mode, then we exit the whole process.
    (when (standalone-application (app *system*))
      (exit 0))))

#+run-example
(run-smiley-example)
