;/* example-start base base.c */
;
;#include <gtk/gtk.h>
;
;int main( int   argc,
;          char *argv[] )
;{
;    GtkWidget *window;
;    
;    gtk_init (&argc, &argv);
;    
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    gtk_widget_show  (window);
;    
;    gtk_main ();
;    
;    return(0);
;}
;/* example-end */
;

(defpackage "02.00-base" (:use :excl :common-lisp))
(in-package "02.00-base")

(defun base ()
  (gtk:gtk_init 0 0)

  (gtk:gtk_widget_show (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))

  #+original
  ;; have to kill lisp to stop
  (gtk:gtk_main)

  #-original (gtk:gtk-main))

;; Notes:
;; -- This example is so basic it doesn't even support bringing the window
;;    down.  You have to kill the Lisp for that.
;;

(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "02.00-base" #'base))