#+(version= 8 2)
(sys:defpatch "xml-rpc" 1
  "v1: apply aserve default external-format to messages & other updates."
  :type :system
  :post-loadable t)

#+(version= 8 1)
(sys:defpatch "xml-rpc" 2
  "v2: allow user method to set faultCode;
 v1: fix require."
  :type :system
  :post-loadable t)

;; -*- mode: common-lisp -*-
;;
;; copyright (c) 2001-2002 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2002-2010 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

;; $Id: xml-rpc.cl,v 2.8 2009/05/01 16:01:11 layer Exp $

(defpackage :net.xml-rpc)
(in-package :net.xml-rpc)

(eval-when (compile load eval)
  (require :aserve (probe-file "sys:;aserve;aserve.fasl"))
  (require :pxml (probe-file "sys:;xmlutils;pxml.fasl"))
  (provide :pxml)
  (provide :xml-rpc))

(defpackage :net.xml-rpc
  (:use :common-lisp :excl
	:net.aserve :net.aserve.client :net.html.generator
	:net.xml.parser)

  (:export

   ;;  CLIENT API:
   #:*xml-rpc-url* 
   #:xml-rpc-call
   #:encode-xml-rpc-call
   #:call-xml-rpc-server

   ;; SERVER API:
   #:xml-rpc-server
   #:xml-rpc-server-name
   #:xml-rpc-export
   #:*xml-rpc-server*
   #:make-xml-rpc-server
   #:enable-xml-rpc-server
   #:disable-xml-rpc-server
   #:export-xml-rpc-method
   #:xml-rpc-method-help
   #:export-standard-xml-rpc-methods
   #:enable-xml-rpc-method
   #:disable-xml-rpc-method

   ;; COMMON API:
   #:xml-rpc-struct
   #:xml-rpc-member
   #:xml-rpc-struct-members
   #:xml-rpc-member-name
   #:xml-rpc-member-value
   #:xml-rpc-slot-value
   #:make-xml-rpc-struct
   #:encode-xml-rpc-value
   #:xml-rpc-encoding
   #:make-xml-rpc-encoding
   #:*xml-rpc-time-zone* ;;; [bug14286]

   #:*xml-rpc-base64*
   #:*xml-rpc-version*
   #:xml-rpc-version

   #:xml-rpc-condition
   #:xml-rpc-fault
   #:xml-rpc-fault-code
   #:xml-rpc-fault-string
   #:xml-rpc-error
   #:xml-rpc-response-error
   #:xml-rpc-argument-error
   #:xml-rpc-error-place
   #:xml-rpc-error-data 

   )
  )

;;; TO DO:
;;; <dateTime.iso8601> what to do with negative year???

;;; See xml-rpc.txt 

(defvar *xml-rpc-url* nil)
(defvar *xml-rpc-server*)
(defvar *xml-rpc-base64* :string)   ;; :string  or  :array
(defvar *xml-rpc-time-zone*
  ;; A value of nil denotes local time -- this was the behavior in 
  ;;   earlier  versions.
  0      ;;; XML-RPC spec specifies UTC [bug14286]
  )

(defvar *xml-rpc-version* 
  '(2 9 7) ;;; apply aserve default external-format to messages [bug19830]
  ;;(2 9 6) ;;; allow conversions between base64 and USB8 array [rfe7489]
  ;;(2 9 5) ;;; convert time values in time-zone 0 (UTC)  [bug14286] 
  ;;(2 9 4) ;;; Allow additional args to pass on to do-http-request [rfe6770] [rfe7564]
  ;;(2 9 3) ;;; use case-sensitive test in xml-rpc-slot-value, add error-p arg [bug15723]
  ;;(2 9 2) ;;; allow class initargs in make-xml-rpc-server [rfe7487]
  ;;(2 9 1) ;;; decode xml-rpc-fault instance signalled by method body [bug19752]
  )

(defun xml-rpc-version (&optional stream)
  (typecase stream
    ((member :number) (apply #'(lambda (a b c) (+ (* 10000 a) (* 100 b) c))
			     *xml-rpc-version*))
    ((or (member nil) stream) (format stream "Allegro CL XML-RPC/~{~A.~A.~A~}"
				      *xml-rpc-version*))
    (otherwise *xml-rpc-version*)))

(defclass xml-rpc-server ()
  ((lock :reader xml-rpc-server-lock :initform (mp:make-process-lock))
   (exports 
    ;; a hashtable of export objects, 
    ;;   key is string name of method
    ;;   value is a list (help-string xml-rpc-export ... )
    :initform (make-hash-table :test #'equalp)
    :accessor xml-rpc-server-exports)
   (parameters   :accessor xml-rpc-server-parameters :initarg :parameters)
   (enabled      :accessor xml-rpc-server-enabled :initform nil)
   (function     :accessor xml-rpc-server-function 
		 :initform 'xml-rpc-server-implementation)
   (export-class :accessor xml-rpc-server-exclass :initform 'xml-rpc-export)
   (name         :reader   xml-rpc-server-name :initarg :name)
   ))

(defclass xml-rpc-export ()
  ((lock :reader xml-rpc-export-lock :initform (mp:make-process-lock))
   (name 
    ;; the name used in the XML protocol string
    :accessor xml-rpc-export-name :initarg :name)
   (function
    :accessor xml-rpc-export-function :initarg :function)
   (result
    ;; the result spec
    :accessor xml-rpc-export-result :initarg :result)
   (args
    ;; the arg specs
    :accessor xml-rpc-export-args :initarg :args)
   (enabled :accessor xml-rpc-export-enabled :initform nil)
   ))


(defclass xml-rpc-struct ()
  ((members :reader xml-rpc-struct-members :initarg :members)  ;; member ...
   ))

(defclass xml-rpc-member ()
  ((name :reader xml-rpc-member-name :initarg :name)
   (lisp-value     :accessor xml-rpc-member-value :initarg :lisp-value :initform nil)
   (encoded-type   :accessor xml-rpc-member-type     :initarg :type :initform nil)
   (encoded-value  :accessor xml-rpc-member-encoding :initform nil)
   (cache-encoding :accessor xml-rpc-member-cache-p  :initarg :cache-p :initform nil)
   ))

(defclass xml-rpc-encoding ()
  ((lisp-value :reader xml-rpc-encoding-value :initarg :lisp-value :initform nil)
   (modifiers  :reader xml-rpc-encoding-modifiers :initarg :modifiers :initform nil)
   (encoding   :reader xml-rpc-encoding :initarg :encoding)))


(defmethod print-object ((x xml-rpc-struct) s)
  (format s "#<xml-rpc-struct ~:{:~A ~S~}>"
	  (mapcar (lambda (m) (list (xml-rpc-member-name m)
				    (xml-rpc-member-value m)))
		  (xml-rpc-struct-members x))))

(defmethod print-object ((m xml-rpc-member) s)
  (format s "#<xml-rpc-member ~A ~S>" 
	  (xml-rpc-member-name m) (xml-rpc-member-value m)))


(define-condition xml-rpc-condition (error) ())

(define-condition xml-rpc-fault (xml-rpc-condition) 
  ((fault-code :reader xml-rpc-fault-code :initarg :fault-code)
   (fault-string :reader xml-rpc-fault-string :initarg :fault-string))
  (:report (lambda (c s)
	     (format s "Received XML-RPC faultCode ~A with description:~%      ~A"
		     (xml-rpc-fault-code c) (xml-rpc-fault-string c)))
	   ))

(define-condition xml-rpc-error (xml-rpc-condition)
  ((place :reader xml-rpc-error-place :initarg :place)
   (data  :reader xml-rpc-error-data  :initarg :data))
  (:report (lambda (c s)
	     (format s "XML-RPC error ~S with data: ~S"
		     (xml-rpc-error-place c) (xml-rpc-error-data c)))
	   ))

(define-condition xml-rpc-response-error (xml-rpc-error)
  ;; place -> :illformed-method-response  :receive-fault
  ;;          :decode-response 
  ())

(define-condition xml-rpc-argument-error (xml-rpc-error)
  ;; place -> :cannot-encode-struct :cannot-encode
  ;;          :make-xml-rpc-server  :export-method-type  
  ())




(defun make-xml-rpc-struct (&rest slots)
  (let (members)
    (do ((tl slots (cddr tl)) name (type nil nil) (cache-p nil nil))
	((atom tl))
      (when (consp (setf name (first tl)))
	(setf type (second name))
	(setf cache-p (third name))
	(setf name (first name)))
      (push (make-instance 'xml-rpc-member 
			   :name (string name)
			   :type type
			   :cache-p cache-p
			   :lisp-value (second tl))
	    members))
    (make-instance 'xml-rpc-struct :members members)))

(defun xml-rpc-slot-name-test (key member &aux (name (xml-rpc-member-name member)))
  ;;  more correct and flexible member name test [bug15723]
  (when (cond ((stringp key) (equal key name))
	      ((not (symbolp key)) 
	       (error "xml-rpc-struct member name must be string or symbol"))
	      ((eq :a :A) (string-equal key name))
	      (t (equal (string key) name)))
    member))

(defmethod xml-rpc-slot-value ((x xml-rpc-struct) name
			       &key (error-p t) ;;; [bug15723]
			       )
  (let* ((members (xml-rpc-struct-members x))
	 (member  (some #'(lambda (m) (xml-rpc-slot-name-test name m))
			members)))
    (cond (member (xml-rpc-member-value member))
	  (error-p (error "Cannot find member ~S ~S" name x))
	  (t nil))))

(defmethod (setf xml-rpc-slot-value) (value (x xml-rpc-struct) name
					    &key (error-p t) ;;; [bug15723]
					    )
  (let* ((members (xml-rpc-struct-members x))
	 (member  (some #'(lambda (m) (xml-rpc-slot-name-test name m))
			members)))
    (cond (member (setf (xml-rpc-member-value member) value
			(xml-rpc-member-encoding member) nil))
	  (error-p (error "Cannot find member ~S ~S" name x))
	  (t nil))))


(defun xml-rpc-call (encoded &rest keys 
			     &key (url *xml-rpc-url*) (agent :aserve a-p) 
			     host headers
			     &aux more)
  ;; Allow additional args to pass on to do-http-request [rfe6770] [rfe7564]
  (do ((tl keys (cddr keys)))
      ((atom tl))
    (flet ((argerr 
	    ()
	    (error "This argument is not allowed in xml-rpc-call: ~S"
		   (car tl))))
      (case (car tl)
	(:method (or (eq :post (cadr tl)) (argerr)))
	(:protocol (or (eq :http/1.0 (cadr tl)) (argerr)))
	(:content-type (or (equal "text/xml" (cadr tl)) (argerr)))
	(:content (argerr))
	(:user-agent (if (and a-p agent) (argerr) (setq agent (cadr tl))))
	((:url :agent :host :headers) nil)
	(otherwise (push (cadr tl) more) (push (car tl) more)))))   
  (multiple-value-bind (s rc)
      (apply #'do-http-request
       url
       :method :post
       :protocol :http/1.0
       :content-type "text/xml"
       :content (format nil "<?xml version=\"1.0\"?>~A" encoded)
       :user-agent agent
       :headers (append (when host `(("Host" . ,host))) headers)
       more)
    (case rc
      (200
       (decode-xml-rpc-response (parse-xml s :content-only t)))
      (otherwise
       (error 'xml-rpc-response-error :place :http-response-code
	      :data (list rc s)))
      )))








(defun encode-xml-rpc-call (name &rest args)
  (format 
   nil 
   "<~A><methodName>~A</methodName><params>~{<param>~A</param>~}</params></~A>"
   "methodCall" name (mapcar #'encode-xml-rpc-value args) "methodCall"))


(defun call-xml-rpc-server (server method &rest args)
  (apply #'xml-rpc-call
	 (apply #'encode-xml-rpc-call method args)
	 server))



(defun make-xml-rpc-encoding (data &rest mods)
  (make-instance 'xml-rpc-encoding 
		 :lisp-value data
		 :modifiers mods
		 :encoding (apply #'encode-xml-rpc-value data mods)))

(defun encode-xml-rpc-value (data &rest mods &aux type tag1 tag2 encoded sanitized)
  ;; The result is a value that will be placed in message with :princ.
  (when mods (setf type (pop mods)))
  (when (or 
	 ;; when data is encoding object, ignore other arguments
	 (typep data 'xml-rpc-encoding)
	 (case type
	   (:encoded  (setf encoded data) 
		      nil)
	   (:string   (setf data (sanitize-html (format nil "~A" data)) sanitized t)
		      t)
	   (:int      (setf data (truncate data))
		      t)
	   (:truncate (setf data (truncate data))
		      t)
	   (:round    (setf data (round data))
		      t)
	   (:double   (setf data (coerce data 'double-float))
		      t)
	   (:boolean (setf tag1 "value"
			   tag2 "boolean"
			   encoded (if data "1" "0"))
		     nil)
	   (:date    (setf tag1 "value"
			   tag2 "dateTime.iso8601"
			   encoded (encode-xml-rpc-date data))
		     nil)
	   (:base64  (setf tag1 "value"
			   tag2 "base64"
			   encoded (encode-base64-data data))
		     nil)
	   (:struct 
	    (typecase data
	      (xml-rpc-struct t)
	      ((member nil)
	       ;; mods -> ((name value [type]) ... )
	       (setf tag1 "value"
		     tag2 "struct"
		     encoded
		     (format nil "~{~A~}"
			     (mapcar #'(lambda (m)
					 (apply #'encode-xml-rpc-value nil :member m))
				     mods))
		     )
	       nil)
	      (otherwise (error 'xml-rpc-argument-error 
				:place :cannot-encode-struct 
				:data  data))))
	   (:member
	    ;; mods -> (name value [type])
	    (setf tag1 "member"
		  tag2 nil
		  encoded
		  (format nil "<name>~A</name>~A"
			  (first mods)
			  (apply #'encode-xml-rpc-value (cdr mods))))
	    nil)
	   (otherwise t)))
    (typecase data
      (string    (setf tag1 "value" tag2 "string" 
		       encoded (if sanitized
				   data
				 (sanitize-html data))))
      (integer   (setf tag1 "value" tag2 "i4")
		 (setf encoded (format nil "~A" data)))
      (float     (setf tag1 "value" tag2 "double")
		 (setf encoded (encode-xml-rpc-double data)))
      (sequence  (setf tag1 "value" tag2 "array")
		 (setf encoded (format nil "<data>~{~A~}</data>"
				       (mapcar #'encode-xml-rpc-value 
					       (concatenate 'list data)))))
      (xml-rpc-encoding (setf tag1 nil tag2 nil
			      encoded (xml-rpc-encoding data)))
      (xml-rpc-member
       (if* (setf encoded (xml-rpc-member-encoding data))
	    then nil
	    else
	    (setf encoded (encode-xml-rpc-value 
			   (xml-rpc-member-value data)
			   (xml-rpc-member-type  data)))
	    (if* (xml-rpc-member-cache-p data)
		 then (setf (xml-rpc-member-encoding data) encoded))))
      (xml-rpc-struct 
       (setf tag1 "value" tag2 "struct")
       (setf encoded (format nil "~{~A~}" (mapcar #'(lambda (m)
						      (encode-xml-rpc-value
						       nil :member
						       (xml-rpc-member-name m)
						       (encode-xml-rpc-value m)
						       :encoded))
						  (xml-rpc-struct-members data)))))
      (otherwise (error 'xml-rpc-argument-error :place :cannot-encode :data  data))
      ))
  (if (or tag1 tag2)
      (format nil "~@[<~A>~]~@[<~A>~]~A~@[</~A>~]~@[</~A>~]" 
	      tag1 tag2 encoded tag2 tag1)
    encoded))


(defun sanitize-html (string)
  (with-output-to-string (s)
      (html-print (list :princ-safe string) s)))


(defun decode-xml-rpc-response (r)
  (multiple-value-bind (v alist)
      (decode-xml-parse 
       r '(("methodResponse" (:or ("fault" :fault)
				  ("params" ("param" :param)))))
       nil :ignore-whitespace)
    (if* (null v)
	 then (error 'xml-rpc-response-error :place :illformed-method-response :data r)
	 elseif (setf v (assoc :fault alist))
	 then (multiple-value-bind (v e)
		  (ignore-errors 
		    (multiple-value-bind (val type)
			(decode-xml-rpc-value (cdr v))
		      (case type
			(:struct 
			 (list :fault-code (xml-rpc-slot-value val "faultCode")
			       :fault-string (xml-rpc-slot-value val "faultString")))
			(otherwise
			 (error "Unxpected fault result ~S ~S" type val))
			)))
		(if v
		    (apply #'error 'xml-rpc-fault v)
		  (error 'xml-rpc-response-error :place :receive-fault :data e)))
	 elseif (setf v (assoc :param alist))
	 then (multiple-value-bind (v e)
		  (ignore-errors 
		    (multiple-value-list (decode-xml-rpc-value (cdr v))))
		(if v 
		    (values-list v)
		  (error 'xml-rpc-response-error :place :decode-response :data e))))))
  
(defun decode-xml-rpc-value (w &aux val type)
  (multiple-value-bind (v alist)
      (decode-xml-parse 
       w
       '(:or
	 ("value" (:or ("int" :int) ("i4" :int) 
		       ("boolean" :boolean) ("double" :float)
		       ("dateTime.iso8601" :date)
		       ("string" :string)
		       ;; allow an empty string [bug13656]
		       ("string" . :string)
		       ("base64" :base64)
		       ;; allow an empty string [bug13656]
		       ("base64" . :base64)
		       ("struct" (:* :members ("member" ("name" :name) :value)))
		       ("array"  ("data" (:* :elements :value)))
			;;; is list of data types complete???
		       :other    ;;; default type is <string>
		       ))
	 ;; allow an empty value as well [bug14302]
	 ("value" . :other))
       nil :ignore-whitespace)
    (if* (null v)
	 then (error "IllFormed XML-RPC value ~S" w)
	 elseif (setf v (assoc :int alist))
	 then (setf type :int val (read-from-string (cdr v)))
	 elseif (setf v (assoc :float alist))
	 then (setf type :double val (decode-xml-rpc-double (cdr v)))
	 elseif (setf v (assoc :string alist))
	 then (setf type :string val
		    (or (cdr v)
			;;  [bug13656] make an empty string if XML was empty
			(make-string 0)))
	 elseif (setf v (assoc :boolean alist))
	 then (setf type :boolean
		    val (eql 1 (read-from-string (cdr v))))
	 elseif (setf v (assoc :date alist))
	 then (setf type :date 
		    val (let* (y m d (h 12) (mn 0) (s 0) 
				 (ds (cdr v)) 
				 (l (length ds)))
			  (setf y (read-from-string ds nil nil :start 0 :end 4)
				m (read-from-string ds nil nil :start 4 :end 6)
				d (read-from-string ds nil nil :start 6 :end 8))
			  (when (and (< 8 l) (equalp #\t (elt ds 8)))
			    (setf h (read-from-string ds nil nil :start 9 :end 11))
			    (when (and (< 11 l) (equalp #\: (elt ds 11)))
			      (setf mn (read-from-string ds nil nil :start 12 :end 14))
			      (when (and (< 14 l) (equalp #\: (elt ds 14)))
				(setf s (read-from-string ds nil nil :start 15 :end 17))
				)))
			  (if (< 1899 y)
			      (if *xml-rpc-time-zone*        ;;; [bug14286]
				  (encode-universal-time s mn h d m y *xml-rpc-time-zone*)
				(encode-universal-time s mn h d m y))
			    (list s mn h d m y))))
	 elseif (setf v (assoc :base64 alist))
	 then  (setf type :base64 
		     val (decode-base64-data
			  ;; [bug13656] decode-base64-string will return an empty
			  ;;   string if value is nil
			  (cdr v)))
	 elseif (setf v (assoc :elements alist))
	 then (setf type :array
		    val (mapcar #'(lambda (v &aux w)
				    (if (setf w (assoc :value v))
					(decode-xml-rpc-value (cdr w))
				      (error "IllFormed XML-RPC array element ~S"
					     w)))
				(cdr v)))
	 elseif (setf v (assoc :members alist))
	 then (setf type :struct
		    val (make-instance 
			 'xml-rpc-struct
			 :members
			 (mapcar #'(lambda (m &aux n v)
				     (if (and (setq n (assoc :name m))
					      (setq v (assoc :value m)))
					 (make-instance 'xml-rpc-member 
							:name (cdr n)
							:lisp-value
							(decode-xml-rpc-value
							 (cdr v)))
				       (error "IllFormed XML-RPC struct member ~S"
					      m)))
				 (cdr v))))
	 

	 elseif (setf v (assoc :other alist))
	 then   (setf type :string val (or (cdr v) (make-string 0)))
	 else (error "IllFormed XML-RPC value ~S" w)
	 )
    (values val type)))

(defun decode-base64-data (string)   ;;; [rfe7489]
  (ecase *xml-rpc-base64*
    (:string (decode-base64-string string))
    (:array (if string
		(base64-string-to-usb8-array string)
	      (make-array 0 :element-type '(unsigned-byte 8))))))

(defun decode-base64-string (string)
  (do ((i 0)
       (len (length string))
       int res j k p s
       )
      ((<= len i)
       (concatenate 'string (nreverse res)))
    (setf j i k 0 p 0)
    (loop 
     (when (or (eql k 4) (eql j len) (eql p 2)) (return))
     (case (excl::base64-digit-char (elt string j))
       (-1 (incf j))
       (-2 (incf j) (incf k) (incf p))
       (otherwise (incf j) (incf k))))
    (setf s (subseq string i j))
    (setf i j)
    ;;(format t "~&k=~S~%" k)
    (case k
      (4 nil)
      (3 (case p
	   (0 (setf p 1 s (concatenate 'string s "=")))
	   (1 (setf p 2 s (concatenate 'string s "=")))
	   (2 nil)))
      (2 (case p
	   (0 (setf p 2 s (concatenate 'string s "==")))
	   (1 (setf p 2 s (concatenate 'string s "=")))
	   (2 nil)))
      (1 (case p
	   (0 (setf p 2 s (concatenate 'string s "==")))
	   (1 (setf k 0)))))
    (case k
      (0 nil)
      (otherwise (setf int (excl::base64-string-to-integer s))
		 (case p
		   (2 (push (code-char (logand #xff int)) res))
		   (1 (push (code-char (ash (logand #xff00 int) -8)) res)
		      (push (code-char (logand #xff int)) res))
		   (0 (push (code-char (ash (logand #xff0000 int) -16)) res)
		      (push (code-char (ash (logand #xff00 int) -8)) res)
		      (push (code-char (logand #xff int)) res)))))
    ))

(defun encode-base64-data (arg)    ;;; [rfe7489]
  (ecase *xml-rpc-base64*
    (:string (encode-base64-string arg))
    (:array (etypecase arg
	      ((array (unsigned-byte 8) (*))
	       (usb8-array-to-base64-string arg))))))
	    

(defun encode-base64-string (arg &aux 
				 (string (typecase arg
					   (string arg)
					   (otherwise (format nil "~A" arg))))
				 (len (length string)) 
				 out)
  (setf out (make-string (* 4 (ceiling len 3))))
  (do ((i 0 (+ i 3))
       (d 0 (+ d 4))
       (int 
	;; init to guard bits in high order pos, 
	;; converts to leading BAAA characters
	#x040000 #x040000) 
       res k
       )
      ((<= len i)
       out)
    (dotimes (j 3)
      (setf k (+ i j))
      (if (< k len)
	  (setf int (+ (ash int 8) (char-int (elt string k))))))
    (setf res (excl::integer-to-base64-string int 48))
    (dotimes (n 4)
      (setf (elt out (+ d n)) (elt res (+ 4 n))))))



(defun decode-xml-parse (parse pattern &optional alist ignore-whitespace)
  ;; pattern -> keyword-var-name | :ignore
  ;;         -> :mw           ;; matches ignored whitespace strings
  ;;         -> "exactMatch"  
  ;;         -> (:or pattern ... )
  ;;         -> (:* keyword-var-name pattern
  ;;         -> (pattern ... )

  (let ((ptail parse) (ttail pattern) match wh)
    (flet ((skip-white (tt)
		       (when (and (consp ptail) 
				  (or ignore-whitespace 
				      (and tt (consp ttail) (eq :mw (car ttail)))))
			 (let (done)
			   (loop
			    (if* (atom ptail)
				 then (return)
				 elseif (not (stringp (car ptail)))
				 then (return)
				 elseif (every #'excl::whitespace-char-p (car ptail))
				 then
				 (setf done (car ptail))
				 (pop ptail)
				 else (return)))
			   (when (and tt (consp ttail) (eq :mw (car ttail)))
			     (setf done t)
			     (pop ttail))
			   done))))


      (if (setf wh (skip-white t))
	  (multiple-value-setq (match alist)
	    (decode-xml-parse ptail ttail alist (when ignore-whitespace wh)))

	(typecase ttail
	  ((member nil) (if (loop
			     (if* (null ptail)
				  then (return t)
				  else (return nil)))
			    (setf match t)))
	  ((member :ignore) (setf match t))
	  ((member :mw) (and (stringp ptail) 
			     (every #'excl::whitespace-char-p ptail)
			     (setf match t)))
	  (keyword (let ((old (assoc ttail alist)))
		     (if old
			 (when (equalp ptail (cdr old)) (setf match t))
		       (setf match t
			     alist (cons
				    (cons ttail
					  (or ptail 
					      (when (stringp ignore-whitespace)
						;; if we are binding to a string
						;; then recover the whitespace
						;; [bug13656]
						ignore-whitespace))
					  )
				    alist)))))
	  (string  (and (or (stringp ptail) (symbolp ptail))
			(string-equal ttail ptail) 
			(setf match t)))
	  (cons (if* (and (consp (car ttail)) (eq :* (caar ttail))) 
		     then
		     (let* ((var (second (car ttail)))
			    (pat (third (car ttail)))
			    found)
		       (loop
			(skip-white nil)
			(when (atom ptail) (return))
			(multiple-value-bind (m a)
			    (decode-xml-parse 
			     (first ptail) pat nil ignore-whitespace)
			  (if* (null m)
			       then (return)
			       else 
			       (push a found)
			       (pop ptail))))
		       (push (cons var (reverse found)) alist))
		     (multiple-value-setq (match alist)
		       (decode-xml-parse ptail (cdr ttail) alist ignore-whitespace))
		     elseif (eq (car ttail) :or)
		     then
		     (dolist (pat (cdr ttail) (values nil alist))
		       (multiple-value-bind (m a)
			   (decode-xml-parse ptail pat alist ignore-whitespace)
			 (setf alist a)
			 (when m (setf match t) (return))))
		     elseif (atom ptail)
		     then nil
		     else
		     (multiple-value-bind (m a)
			 (decode-xml-parse 
			  (car ptail) (car ttail) alist ignore-whitespace)
		       (setf alist a)
		       (if m
			   (multiple-value-setq (match alist)
			     (decode-xml-parse 
			      (cdr ptail) (cdr ttail) alist ignore-whitespace))))))
	  ))
      (values match alist))))



(defmethod xml-rpc-server-methods ((x xml-rpc-server) name &optional all)
  (mp:with-process-lock
   ((xml-rpc-server-lock x))
   (let ((methods (gethash (string name) (xml-rpc-server-exports x))))
     (if all
	 (cdr methods)
       (mapcan #'(lambda (m) (and (xml-rpc-export-enabled m) (list m))) 
	       (cdr methods))))))

(defmethod xml-rpc-server-method ((x xml-rpc-server) name arg-sig &optional all)
  ;; xml-rpc types are disjoint so signatures do not form a lattice with
  ;;    any depth
  (mp:with-process-lock
   ((xml-rpc-server-lock x))
   (dolist (m (xml-rpc-server-methods x name all) nil)
     (and (or all (xml-rpc-export-enabled m))
	  (equalp arg-sig (xml-rpc-export-args m))
	  (return m)))))



(defun make-xml-rpc-server (&key start 
				   (enable t) (introspect t)
				   publish 
				   (class 'xml-rpc-server)
				   (name 
				    (format 
				     nil 
				     "AllegroServe/~{~A.~A.~A~}(Allegro Common Lisp)"
				     *aserve-version*))
				   )
  (or (member :path publish) (setq publish (list* :path "/RPC2" publish)))
  (when (or (member :function publish) (member :content-type publish))
    (error 'xml-rpc-argument-error :place :make-xml-rpc-server
	    :data "does not allow :function or :content-type arguments"))
  (let ((server (apply #'make-instance 
		       (if (consp class) (first class) class)   ;;; [rfe7487]
		       :parameters publish :name name
		       (when (consp class) (cdr class))         ;;; [rfe7487]
		       )))
    (when start 
      (apply #'start (when (consp start) start)))
    (when enable (enable-xml-rpc-server server))
    (when introspect (export-standard-xml-rpc-methods server enable))
    server))

(defmethod enable-xml-rpc-server ((server xml-rpc-server) &optional enable-exports)
  (mp:with-process-lock 
   ((xml-rpc-server-lock server))
   (setf (xml-rpc-server-enabled server) t)
   (when enable-exports
     (maphash #'(lambda (k v)
		  (declare (ignore v))
		  (enable-xml-rpc-method server k :all))
	      (xml-rpc-server-exports server)))
   (apply #'publish     
	  :function #'(lambda (req ent) 
			(let ((*xml-rpc-server* server))
			  (funcall (xml-rpc-server-function *xml-rpc-server*) req ent)))
	  :content-type "text/xml"
	  (xml-rpc-server-parameters server))
   ;; return server object
   server))

(defmethod disable-xml-rpc-server ((server xml-rpc-server))
  ;; unpublish???
  (setf (xml-rpc-server-enabled server) nil)
  server)


(defun xml-rpc-server-implementation (request entity)
  ;; parse an XML rpc call and pass it to the exported function
  (when (xml-rpc-server-enabled *xml-rpc-server*)
    (let* ((body (get-request-body request
				   ;; If we do not specify an external format, Aserve
				   ;; uses :octets but here we want to do any requested
				   ;; character conversions   [bug19830]
				   :external-format   
				   net.aserve:*default-aserve-external-format*
				   ))
	   code string rval params pvals ptypes)
      (multiple-value-bind (name e)
	  (ignore-errors
	    (multiple-value-list
	     (let* ((xml (parse-xml body :content-only t))
		    name v alist w)
	       (multiple-value-setq (v alist)
		 (decode-xml-parse
		  xml '(("methodCall"
			 ("methodName" :name)
			 .
			 (:or (("params" (:* :params ("param" :param))))
			      nil)))
		  nil :ignore-whitespace))
	       (or (and v
			(setf name (assoc :name alist))
			(setf name (cdr name)))
		   (error "missing methodName"))
	       ;; <params> may be omitted if empty
	       (setf params (assoc :params alist))
	       (dolist (p (cdr params))
		 (or (and p
			  (setf w (assoc :param p))
			  (multiple-value-bind (val type)
			      (decode-xml-rpc-value (cdr w))
			    (when type
			      (push val pvals)
			      (push type ptypes)
			      t)))
		     (error "param ~S" p)))
	       (setf pvals (reverse pvals))
	       (setf ptypes (reverse ptypes))
	       name)))
	(if* e 
	     then
	     (typecase e
	       (xml-rpc-fault
		;; The exported method body signalled an error of the form
		;;  (error 'xml-rpc-fault :fault-code c :fault-string s)
		;;  [bug19752]
		(setf code (xml-rpc-fault-code e)
		      string (xml-rpc-fault-string e)))
	       (otherwise
		(setf code 2
		      string (format nil "Error in XML-RPC call: ~A" e))))
	     (setf code 1
		   string 
		   (format nil "Ill-formed XML-RPC call: ~A" e))	   
	     else
	     (setf name (car name))
	     (multiple-value-bind (v e)
		 (ignore-errors
		   (multiple-value-list
		    (let* ((sig ptypes)
			   (entry   (xml-rpc-server-method *xml-rpc-server* name sig))
			   (function (and entry 
					  ;; is this mp safe???
					  (xml-rpc-export-function entry))))
		      (or function (error "Unknown XML-RPC method ~A(~{ ~A~})"
					  name sig))
		      (encode-xml-rpc-value (apply function pvals)
					    (xml-rpc-export-result entry))
		      )))
	       (if* e
		    then
		    (setf code 2
			  string (format nil "Error in XML-RPC call: ~A" e))
		    else
		    ;; emit result
		    (setf rval (first v))
		    )))
	(with-http-response 
	 (request entity)
	 ;; must send content-length [spr28777] [bug14271]
	 (if* (equal (request-reply-strategy request) '(:use-socket-stream))
	      then (setf (request-reply-strategy request)
			 '(:string-output-stream :post-headers))) 
	 (with-http-body 
	  (request entity 
		   :headers `((:server . ,(xml-rpc-server-name *xml-rpc-server*))))
	  (html
	   (:princ "<?xml version=\"1.0\"?>")
	   (:princ "<methodResponse>")

	   (if* code
		then
		;; emit fault
		(html
		 (:princ "<fault>"
			 (encode-xml-rpc-value nil :struct
					       (list "faultCode" code)
					       (list "faultString" string))
			 "</fault>"))
		else
		;; emit value
		(html
		 (:princ "<params><param>"
			 rval
			 "</param></params>"))
		)

	   (:princ "</methodResponse>")
	   )))))))


(defmethod export-xml-rpc-method ((server xml-rpc-server) 
				  name-spec ;; (rpcname lispname enable helpstring)
				  result-spec &rest arg-specs)
  (mp:with-process-lock 
   ((xml-rpc-server-lock server))
   (let* ((name     (if (consp name-spec) 
			(first name-spec) 
		      name-spec))
	  (function (if (consp name-spec) 
			(or (second name-spec) (first name-spec))
		      name))
	  (enable   (if (and (consp name-spec) (cddr name-spec))
			(third name-spec)
		      t))
	  (help     (if (consp name-spec) (fourth name-spec)))
	  (key (string name))
	  (exports (xml-rpc-server-exports server))
	  (entries (gethash key exports))
	  (entry   (xml-rpc-server-method server name arg-specs :all)))
     (or (symbolp function) (functionp function)
	 (error 'xml-rpc-argument-error :place :export-method-type :data function))
     (if* (null entries)
	  then 
	  (setf entries (list (or help "") nil))
	  (setf (gethash key exports) entries)
	  (pop entries)
	  else
	  (when help (setf (car entries) help))
	  (if* entry
	       then
	       (setf entries (member entry entries))
	       else
	       (push nil (cdr entries))
	       (pop entries)))
     (setf entry (make-instance (xml-rpc-server-exclass server)
			    :name (string name)
			    :function function
			    :result result-spec
			    :args arg-specs))
     (setf (car entries) entry)
     (when enable (setf (xml-rpc-export-enabled entry) t))
     key)))

(defmethod xml-rpc-method-help ((server xml-rpc-server) name)
  (mp:with-process-lock 
   ((xml-rpc-server-lock server))
   (let* ((key (string name))
	  (exports (xml-rpc-server-exports server))
	  (entries (gethash key exports)))
     (if entries (car entries) ""))))

(defmethod (setf xml-rpc-method-help) (help-string (server xml-rpc-server) name)
  (mp:with-process-lock 
   ((xml-rpc-server-lock server))
   (let* ((key (string name))
	  (exports (xml-rpc-server-exports server))
	  (entries (gethash key exports)))
     (setf help-string (typecase help-string
		       (string help-string)
		       (otherwise "")))
     (if* entries
	  then (setf (car entries) help-string)
	  else (setf (gethash key exports) (setf entries (list help-string)))
	  )
     help-string)))

(defmethod enable-xml-rpc-method ((server xml-rpc-server) name &rest args-specs)
  ;; if arg-specs is :all then enable all signatures
  (mp:with-process-lock 
   ((xml-rpc-server-lock server))
   (let ((ex (gethash (string name) (xml-rpc-server-exports server))))
     (dolist (e (cdr ex))
       (when (or (eq (first args-specs) :all)
		 (equal args-specs (xml-rpc-export-args e)))
	 (setf (xml-rpc-export-enabled e) t)))
     (string name))))

(defmethod disable-xml-rpc-method ((server xml-rpc-server) name &rest args-specs)
  ;; if arg-specs is :all then disable all signatures
  (mp:with-process-lock 
   ((xml-rpc-server-lock server))
   (let ((ex (gethash (string name) (xml-rpc-server-exports server))))
     (dolist (e (cdr ex))
       (when (or (eq (first args-specs) :all)
		 (equal args-specs (xml-rpc-export-args e)))
	 (setf (xml-rpc-export-enabled e) nil)))
     (string name))))


(defmethod export-standard-xml-rpc-methods ((server xml-rpc-server) 
					    &optional (enable t))

  ;; define some commonly expected instrospection methods

  (export-xml-rpc-method server 
    `("system.listMethods" xml-rpc-system-list-methods ,enable
      "Returns an array of the names of all the exported methods on the server.")
    :array)
  
  (export-xml-rpc-method server 
    `("system.methodSignature" xml-rpc-system-method-signature ,enable
      "Returns an array of signature arrays: ((return-type arg-type ...) ...) ")
    :array :string)
  
  (export-xml-rpc-method server 
    `("system.methodHelp" xml-rpc-system-method-help ,enable
      "Returns a string describing a method.")
    :string :string)
  
  )

(defun xml-rpc-system-list-methods ()
  ;; returns array of method names
  (mp:with-process-lock 
   ((xml-rpc-server-lock *xml-rpc-server*))
   (let (names)
     (maphash #'(lambda (k v) 
		  (and (some #'xml-rpc-export-enabled (cdr v)) (push k names)))
	      (xml-rpc-server-exports *xml-rpc-server*))
     (sort names #'string-lessp))))

(defun xml-rpc-system-method-signature (name)
  ;; returns array of ret + arg ...
  (mp:with-process-lock 
   ((xml-rpc-server-lock *xml-rpc-server*))
   (let* ((entries (xml-rpc-server-methods *xml-rpc-server* name))
	  (sigs
	   (mapcar #'(lambda (e) 
		       (mapcar
			#'string
			(list* (xml-rpc-export-result e)
			       (xml-rpc-export-args e))))
		   entries)))
     sigs)))

(defun xml-rpc-system-method-help (name)
  ;; returns string
  (xml-rpc-method-help *xml-rpc-server* name))






(defun encode-xml-rpc-double (f)
  ;; Force the number to be printed as digits.digits
  (if (or (eql f 0e1) (eql f 0d1))
      (concatenate 'string "0.0")   ;; bug13943
    (string-trim
     " "
     (format nil "~VF" 
	     (+ 
	      ;; This is conservatively the most significant digits
	      ;; in a double-float number, plus room for a leading or
	      ;; trailing zero.
	      22
	      ;; This is conservatively the most leading or trailing
	      ;; zeroes that will be printed.
	      (abs (truncate (log (abs f) 10))))
	     f))))


(defun decode-xml-rpc-double (s) 
  (read-from-string 
   ;; Make sure that the data will be parsed as a double-float
   (concatenate 'string s "d0")))


(defun encode-xml-rpc-date (data)
  (multiple-value-bind (s m h dy mo yr)
      (typecase data
	(cons (values-list data))
	(integer 
	 (if *xml-rpc-time-zone*                 ;;; [bug14286]
	     (decode-universal-time data *xml-rpc-time-zone*)
	   (decode-universal-time data)))
	((member nil :now) (get-decoded-time))
	(otherwise (error 'xml-rpc-argument-error 
			  :place :cannot-encode-date :data data)))
    (format nil 
	    "~4,'0D~2,'0D~2,'0DT~2,'0D:~2,'0D:~2,'0D"
	    yr mo dy h m s)))
