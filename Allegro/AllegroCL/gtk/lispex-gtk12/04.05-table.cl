;/* example-start table table.c */
;
;#include <gtk/gtk.h>
;
;/* Our callback.
; * The data passed to this function is printed to stdout */
;void callback( GtkWidget *widget,
;               gpointer   data )
;{
;    g_print ("Hello again - %s was pressed\n", (char *) data);
;}
;
;/* This callback quits the program */
;gint delete_event( GtkWidget *widget,
;                   GdkEvent  *event,
;                   gpointer   data )
;{
;    gtk_main_quit ();
;    return(FALSE);
;}
;
;int main( int   argc,
;          char *argv[] )
;{
;    GtkWidget *window;
;    GtkWidget *button;
;    GtkWidget *table;
;
;    gtk_init (&argc, &argv);
;
;    /* Create a new window */
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;
;    /* Set the window title */
;    gtk_window_set_title (GTK_WINDOW (window), "Table");
;
;    /* Set a handler for delete_event that immediately
;     * exits GTK. */
;    gtk_signal_connect (GTK_OBJECT (window), "delete_event",
;                        GTK_SIGNAL_FUNC (delete_event), NULL);
;
;    /* Sets the border width of the window. */
;    gtk_container_set_border_width (GTK_CONTAINER (window), 20);
;
;    /* Create a 2x2 table */
;    table = gtk_table_new (2, 2, TRUE);
;
;    /* Put the table in the main window */
;    gtk_container_add (GTK_CONTAINER (window), table);
;
;    /* Create first button */
;    button = gtk_button_new_with_label ("button 1");
;
;    /* When the button is clicked, we call the "callback" function
;     * with a pointer to "button 1" as its argument */
;    gtk_signal_connect (GTK_OBJECT (button), "clicked",
;              GTK_SIGNAL_FUNC (callback), (gpointer) "button 1");
;
;
;    /* Insert button 1 into the upper left quadrant of the table */
;    gtk_table_attach_defaults (GTK_TABLE(table), button, 0, 1, 0, 1);
;
;    gtk_widget_show (button);
;
;    /* Create second button */
;
;    button = gtk_button_new_with_label ("button 2");
;
;    /* When the button is clicked, we call the "callback" function
;     * with a pointer to "button 2" as its argument */
;    gtk_signal_connect (GTK_OBJECT (button), "clicked",
;              GTK_SIGNAL_FUNC (callback), (gpointer) "button 2");
;    /* Insert button 2 into the upper right quadrant of the table */
;    gtk_table_attach_defaults (GTK_TABLE(table), button, 1, 2, 0, 1);
;
;    gtk_widget_show (button);
;
;    /* Create "Quit" button */
;    button = gtk_button_new_with_label ("Quit");
;
;    /* When the button is clicked, we call the "delete_event" function
;     * and the program exits */
;    gtk_signal_connect (GTK_OBJECT (button), "clicked",
;                        GTK_SIGNAL_FUNC (delete_event), NULL);
;
;    /* Insert the quit button into the both 
;     * lower quadrants of the table */
;    gtk_table_attach_defaults (GTK_TABLE(table), button, 0, 2, 1, 2);
;
;    gtk_widget_show (button);
;
;    gtk_widget_show (table);
;    gtk_widget_show (window);
;
;    gtk_main ();
;
;    return 0;
;}
;/* example-end */

(defpackage "04.05-table" (:use :excl :common-lisp))
(in-package "04.05-table")

(ff:defun-foreign-callable callback ((widget (* gtk:GtkWidget))
				     (data gtk:gpointer))
  (declare (ignore widget))
  (format t "~&Hello again - ~a was pressed~%"
	  (native-to-string data :external-format gtk:gpointer-to-string-ef))
  (values))

(ff:defun-foreign-callable delete-event ((widget (* gtk:GtkWidget))
					 (event (* gtk:GdkEvent))
					 (data (* gtk:gpointer)))
  (declare (ignore widget event data))
  (format t "~&Delete Event Called~%")
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit)
  gtk:FALSE)

(defun table ()
  (let ((window nil)
	(button nil)
	(table nil)
	(delete-event-cb (ff:register-foreign-callable 'delete-event))
	(callback-cb (ff:register-foreign-callable 'callback)))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))

    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Table")

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "delete_event"
			    (gtk:GTK_SIGNAL_FUNC delete-event-cb) gtk:NULL)

    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 20)
    
    (setq table (gtk:gtk_table_new 2 2 gtk:TRUE))
    
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) table)
    
    (setq button (gtk:gtk_button_new_with_label "button 1"))
    
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"
			    (gtk:GTK_SIGNAL_FUNC callback-cb) "button 1")

    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 0 1 0 1)
    
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_button_new_with_label "button 2"))

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"
			    (gtk:GTK_SIGNAL_FUNC callback-cb) "button 2")

    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 1 2 0 1)

    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_button_new_with_label "Quit"))

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"
			    (gtk:GTK_SIGNAL_FUNC delete-event-cb) gtk:NULL)

    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 0 2 1 2)

    (gtk:gtk_widget_show button)
    (gtk:gtk_widget_show table)
    (gtk:gtk_widget_show window)
    #+original (gtk:gtk_main)
    #-original (gtk:gtk-main)))


(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "04.05-table" #'table))
