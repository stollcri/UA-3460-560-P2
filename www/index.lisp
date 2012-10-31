;; modlisp-cmucl.lisp
;; original mod-lisp.lisp author marc.battyani@fractalconcept.com
;; ported to cmucl by dig@mail.com

;; To test it eval this:
;; (start-apache-listener)

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
    ((html
      (get-html-content (subseq (cdr (assoc "url" command :test #'string=)) 5))
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

(defun parse-url-query-worker (query-string)
  (if (position #\= query-string)
    ; there is a variable/value delimiter
    (list ; lists are nested to create a list of lists in parse-url-query
      (list
        (subseq query-string 0 (position #\= query-string))
        (subseq query-string (+ 1 (position #\= query-string)))
      )
    )
    ; there is no variable/value delimiter (value is blank)
    (list ; lists are nested to create a list of lists in parse-url-query
      (list
        (subseq query-string 0 (position #\= query-string))
        ""
      )
    )
  )
)

(defun parse-url-query (query-string)
  (if (= (length query-string) 0)
    ; the query string is empty
    NIL
    ; the query string is not empty
    (if (position #\& query-string)
      ; there are multiple url query variables
      (append ; use append to create list of lists
        (parse-url-query-worker
          (subseq query-string 0 (position #\& query-string))
        )
        (parse-url-query
          (subseq query-string (+ 1 (position #\& query-string)))
        )
      )
      ; there is only one url query variable
      (parse-url-query-worker query-string)
    )
  )
)

(defun get-html-content (page-name)
  (if (position #\? page-name)
    ; we have a query string to parse
    (html-dynamic
      (subseq page-name 0 (position #\? page-name))
      (parse-url-query (subseq page-name (+ 1 (position #\? page-name))))
    )
    ; no query string pass page name (with leading "/")
    ;(html-fixed page-name)
    (html-dynamic page-name "")
  )
)

(defun html-head ()
  "<!doctype html>
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" dir=\"ltr\">
    <head><meta charset=\"utf-8\" /><title>Clisp Sample WebApp</title>
    </head><body>"
)

(defun html-tail ()
  "</body></html>"
)

(defun html-dynamic (page-name query-string)
  (if (string= page-name "/nlp")
    (html-content-nlp query-string)
    ; show the default page
    (with-output-to-string (s)
      (write-string (html-head) s)
      (write-string
        "<h1>mod_lisp 2.0</h1>
          <p>This is a default html string sent by CMUCL + mod_lisp 2.0 + Apache2 + OS X</p>"
        s
      )
      (format s "<p>Page name: ~a</p>" page-name)
      (format s "<p>Query string: ~a</p>" query-string)
      (write-string (html-tail) s)
    )
  )
)

(defun html-content-nlp (query-string)
  (with-output-to-string (s)
    (write-string (html-head) s)
    (write-string
      "<h1>Natural Language Processing</h1>
        <p>Something goes here</p>"
      s
    )
    (format s "<p>Query string: ~a</p>" query-string)
    (write-string (html-tail) s)
  )
)
