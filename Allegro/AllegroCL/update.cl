;; $Id: update.cl,v 1.9.2.1 2007/07/25 06:35:01 layer Exp $

(require :winapi)
(require :winapi-dev)

(setq excl::*restart-init-function* nil) ;; prevent IDE from starting

(ff:def-foreign-type _system_info
    (:struct (:union (dwOemId win:dword)
		     (:struct (wProcessorArchitecture win:word)
			      (wReserved win:word)))
	     (dwPageSize win:dword)
	     (lpMinimumApplicationAddress win:lpvoid)
	     (lpMaximumApplicationAddress win:lpvoid)
	     ;; (dwActiveProcessorMask win::dword_ptr)
	     (dwActiveProcessorMask (* win:dword))
	     (dwNumberOfProcessors win:dword)
	     (dwProcessorType win:dword)
	     (dwAllocationGranularity win:dword)
	     (wProcessorLevel win:word)
	     (wProcessorRevision win:word)))

(ff:def-foreign-call (get-system-info "GetSystemInfo")
    ((lpSystemInfo (* _system_info)))
  :returning :void)

(defun number-of-processors ()
  (let ((obj (ff:allocate-fobject '_system_info :c)))
    (get-system-info obj)
    (ff:fslot-value-typed '_system_info :c obj 'dwNumberOfProcessors)))

(progn
  (defvar error-detected nil)
  (defvar do-bundle-update t)

  (format t "~%~%
This Lisp will exit after rebuilding images, unless an error occurs.~%~%")
  
  (defun print-bli-form (file build debug s &aux (*print-case* :downcase))
    (print
     `(handler-case
	  (progn
	    (setq *print-startup-message*
	      (let ((r (reverse *print-startup-message*)))
		(reverse
		 (pushnew '(:external-format . t) r
			  :test #'equal))))
	    (build-lisp-image
	     ,file
	     :verbose t
	     :pll-from-sys t
	     :pre-dump-form
	     `(progn
		(dolist (sl excl::.loaded-shared-libraries.)
		  (when (equalp "aclre32.dll"
				(file-namestring (excl::shlib-name sl)))
		    (setf (excl::shlib-name sl)
		      (merge-pathnames "aclre32.dll" #p"sys:;cg;")))))
	     :build-input ,debug
	     :build-output ,build
	     #+mswindows :splash-from-file
	     #+mswindows (translate-logical-pathname "sys:allegro.dib")))
	(error () (exit 1)))
     s)
    (print '(exit 0) s)
    (terpri s))

  (dolist (dxl-file (directory "*.dxl"))
    (let ((file (file-namestring dxl-file))
	  (dxl-file (namestring dxl-file)))
      (if* (not (member file '("alisp.dxl" "alisp8.dxl" "mlisp.dxl"
			       "mlisp8.dxl"
			       "allegro.dxl" "allegro-ansi.dxl"
			       "allegro-express.dxl"
			       "clim.dxl")
			:test #'string-equal))
	 thenret ;; ignore these files
	 else (format t "Making new ~a...~%" file)
	      (let ((orig-dxl (format nil "~aorig.dxl" (pathname-name file)))
		    (old-dxl (format nil "~aold.dxl" (pathname-name file)))
		    (build (format nil "~a.build" (pathname-name file)))
		    (debug (format nil "~a.debug" (pathname-name file)))
		    (lisp-exe (pathname-name file))
		    prev)

		(when do-bundle-update 
		  (setq do-bundle-update nil)
		  (format t "~
Performing bundle check.
The bundle check may take several minutes.")
		  (with-open-file (s "bunup-build.cl" :direction :output
				   :if-exists :supersede)
		    (format s "~
 (setq excl::*restart-init-function* nil) ;; prevent IDE from starting
 (build-lisp-image \"tmpbu.dxl\" :verbose t :build-output \"tmpbu.build\"
                   :include-devel-env t :build-input \"tmpbu.debug\"
                   :include-ide nil :restart-app-function nil
                   :restart-init-function nil)
 (exit 0)"))
		  (run-shell-command (format nil "~
~a +R +B +cn -I ~a -L bunup-build.cl -qq -batch -backtrace-on-error"
					     lisp-exe file)
				     :show-window :minimized)
		  (with-open-file (s "bunup-check.cl" :direction :output
				   :if-exists :supersede)
		    (format s "~
 (progn
   (setq excl::*restart-init-function* nil) ;; prevent IDE from starting
   (when (fboundp 'excl::update-bundle-check)
     (unless (excl::update-bundle-check t)
       (excl::update-bundle-files)))
   (exit 0))
"))
		  (run-shell-command (format nil "~
~a +R +B +cn -I tmpbu.dxl -L bunup-check.cl -qq -batch -backtrace-on-error"
					     lisp-exe)
				     :show-window :minimized)
		  (when (sys:getenv "ACL_UPDATE_SLEEP_HACK") (sleep 10))
		  (when (> (number-of-processors) 1) (sleep 5))
		  (delete-file "tmpbu.dxl")
		  (format t "~&Finished bundle check.~%"))
		

		(if* (not (probe-file orig-dxl))
		   then (setq prev (rename-file-raw dxl-file orig-dxl))
		   else (when (probe-file old-dxl) (delete-file old-dxl))
			(setq prev (rename-file-raw dxl-file old-dxl)))
		(with-open-file (s "update1.cl" :direction :output
				 :if-exists :supersede)
		  (print-bli-form file build debug s))
		(when (/= 0
			  (run-shell-command
			   (format nil
				   "~a +R +B +cn +s update1.cl -I ~a -qq ~a"
				   lisp-exe orig-dxl
				   "-batch -backtrace-on-error")
			   :show-window :minimized))
		  (setq error-detected dxl-file)
		  (when (probe-file dxl-file) (delete-file dxl-file))
		  (rename-file-raw prev dxl-file))
		(delete-file "update1.cl")
		(when (or error-detected (not (probe-file file)))
		  (format t "Build of ~a failed (check ~a for errors).~%"
			  file build)
		  (return))))))

  (if* error-detected
     then (let ((msg (format nil "Update failed creating ~a" error-detected)))
	    (format t "~a.~%" msg)
	    (excl::internal-message-box msg "Allegro CL update"))
     else (format t "Done.~%"))
  (values))

(exit 0)
