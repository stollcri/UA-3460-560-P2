;; modlisp-cmucl.lisp
;; original mod-lisp.lisp author marc.battyani@fractalconcept.com
;; ported to cmucl by dig@mail.com

;; To test it eval this:
;; (start-apache-listener)
;; (fetch-mod-lisp-url "localhost" "/asp/fixed")

(defconstant +apache-port+ 3000)
(defvar *apache-stream* nil) ;the socket to apache
(defvar *close-apache-stream* nil) ;set to t if you want to close the socket to apache
(defvar *apache-nb-use-socket* 0) ;the number of requests sent in this socket

(defun make-apache-listener (port)
  (let
    ((socket (ext:create-inet-listener port)))
    (unwind-protect
      (loop
        (mp:process-wait-until-fd-usable socket :input)
        (multiple-value-bind
          (new-fd remote-host)
          (ext:accept-tcp-connection socket)
          (let ((stream (sys:make-fd-stream new-fd :input t :output t)))
            (mp:make-process #'(lambda () (apache-listen stream)))
          )
        )
      )
      (unix:unix-close socket)
    )
  )
)

(defun start-apache-listener ()
  (mp:make-process #'(lambda () (make-apache-listener +apache-port+)))
)

(defun apache-listen (*apache-stream*)
  (let
    ((*close-apache-stream* t))
    (unwind-protect
      (loop for *apache-nb-use-socket* from 0
        for command = (get-apache-command)
        while command do
          (process-apache-command command)
          (force-output *apache-stream*)
        until *close-apache-stream*
      )
      (close *apache-stream*)
    )
  )
)

(defun get-apache-command ()
  (ignore-errors
    (let*
      ((header (loop for key = (read-line *apache-stream* nil nil)
        while (and key (string-not-equal key "end"))
        for value = (read-line *apache-stream* nil nil)
        collect (cons key value)))
        (content-length (cdr (assoc "content-length" header :test #'equal)))
        (content
          (when content-length
            (make-string (parse-integer content-length :junk-allowed t))
          )
        )
      )
      (when content
        (read-sequence content *apache-stream*)
        (push (cons "posted-content" content) header)
      )
      header
    )
  )
)

(defun write-header-line (key value)
  (write-string key *apache-stream*)
  (write-char #\NewLine *apache-stream*)
  (write-string value *apache-stream*)
  (write-char #\NewLine *apache-stream*)
)

(defun process-apache-command (command)
  (let
    ;((template-name
      ;(subseq (cdr (assoc "url" command :test #'string=)))) 6)
      ;(template-from-query (subseq (cdr (assoc "url" command :test #'string=)))) 6)
    ;))
    ((html
      (if (equal (cdr (assoc "url" command :test #'string=)) "/lisp/system-info")
        (debug-table command)
        (if (equal (subseq (cdr (assoc "url" command :test #'string=)) 6) "test") ; /listp/
          (fixed-html)
          (debug-table command)
        )
      )
    ))
    (write-header-line "Status" "200 OK")
    (write-header-line "Content-Type" "text/html")
    (write-header-line "Content-Length" (format nil "~d" (length html)))
    (write-header-line "Keep-Socket" "1")
    (write-string "end" *apache-stream*)
    (write-char #\NewLine *apache-stream*)
    (write-string html *apache-stream*)
    (setf *close-apache-stream* nil)
  )
)

(defun debug-table (command)
  (with-output-to-string (s)
    (write-string
      "<!doctype html>
       <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" dir=\"ltr\">
       <head><meta charset=\"utf-8\" /><title>Clisp Sample WebApp</title>
       </head><body>
        <table bgcolor=\"#c0c0c0\">
        <tr bgcolor=\"yellow\"><th colspan=2>CMUCL + mod_lisp 2.0 + Apache2 + OS X</th></tr>
        <tr bgcolor=\"yellow\"><th>Key</th><th>Value</th></tr>"
      s
    )
    (format s "<tr bgcolor=\"#F0F0c0\"><td>apache-nb-use-socket</td><td>~a</td></tr>"  *apache-nb-use-socket*)
    (loop for (key . value) in command do
      (format s "<tr bgcolor=\"#F0F0c0\"><td>~a</td><td>~a</td></tr>" key value)
    )
    (write-string
      "</table>"
      s
    )
    (format s "<p>~a</p>" command)
    (write-string
      "</body></html>"
      s
    )
  )
)

(defun fixed-html ()
  "<!doctype html>
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" dir=\"ltr\">
    <head><meta charset=\"utf-8\" /><title>Clisp Sample WebApp</title>
    </head><body>
      <h1>mod_lisp 2.0</h1>
      <p>This is a constant html string sent by CMUCL + mod_lisp 2.0 + Apache2 + OS X</p>
      <p>~a</p>
      <a href=\"/lisp/system-info\">Debug Information</a>
    </body></html>"
)

(defun fetch-mod-lisp-url (server url &key (nb-fetch 1) (port 3000) close-socket)
  (let
    ((socket (ext:connect-to-inet-socket server port)) (reply))
    (unwind-protect
      (let
        ((stream (sys:make-fd-stream socket :input t :output t)))
        (dotimes
          (i nb-fetch)
          (write-string "url" stream)
          (write-char #\NewLine stream)
          (write-string url stream)
          (write-char #\NewLine stream)
          (write-string "end" stream)
          (write-char #\NewLine stream)
          (force-output stream)
          (setf reply (read-reply stream))
          (when close-socket
            (close stream)
            (setf stream nil)
          )
        )
      )
      (unix:unix-close socket)
    )
    reply
  )
)

(defun read-reply (socket)
  (let*
    (
      (header
        (loop for key = (read-line socket nil nil)
           while (and key (string-not-equal key "end"))
           for value = (read-line socket nil nil)
           collect (cons key value)
        )
      )
      (content-length (cdr (assoc "Content-Length" header :test #'string=)))
      (content (when content-length (make-string (parse-integer content-length :junk-allowed t))))
    )
    (when content
      (read-sequence content socket)
      (push (cons "reply-content" content) header)
    )
    header
  )
)