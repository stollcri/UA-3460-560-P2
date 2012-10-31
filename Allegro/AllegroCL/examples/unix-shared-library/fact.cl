;; This code is in the public domain.
;; $Id: fact.cl,v 1.5 2008/04/17 00:36:33 duane Exp $

(in-package :user)

(defun factorial (x)			; assumes caller will trap errors
  (cond
   ((= x 0) 1)
   (t (* x (factorial (1- x))))))

(ff:defun-foreign-callable factorial-callback ((arg :unsigned-long))
  (force-output *error-output*)
  (string-to-native (format nil "~s" (factorial arg))))

;; perform whatever setup is necessary, then return.

;; foreign callback addresses are static between restarts, 
;; so we can register our API at build/load time.
(ff:register-foreign-callable #'factorial-callback nil t)

(defun initialize-factorial ()
  ;; user should perform app specific initialization here,
  ;; command-line argument processing, etc.
  ;; but, there's nothing to initialize for this example.
  )

(setf excl:*restart-app-function* #'initialize-factorial)
