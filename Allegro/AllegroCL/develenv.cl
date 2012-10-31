;; $Id: develenv.cl,v 1.19 2005/08/03 05:06:02 layer Exp $
;;
;; build-lisp-image uses this file to load the "development environment"
;; (the :include-devel-env keyword).
;;
;; If you are using Allegro CL Runtime or Dynamic Runtime, you cannot
;; include any of the following modules:
;;   :xref
;;   :step
;;   :disasm
;;   :prof

(require :list2)
(require :seq2)
(require :safeseq)
(require :regexp)
;;(require :streama)
(require :srecord)
(require :tpl-debug)
(require :tpl-proc)
(require :defsys)
(require :foreign)
(require :defftype)
(require :process)
;; The features in MS Windows do not exist for us to implement the SIGIO
;; facility:
#-mswindows (require :sigio)
(require :excl)
(require :eli)
(require :emacs)
(require :lze)
(require :lep)
(require :scm)
(require :xref) ;; not allowed in runtime images
;;(require :walker)
(require :trace)
(require :prof) ;; not allowed in runtime images
(require :inspect)
(require :disasm) ;; not allowed in runtime images
(require :sock)
(require :loop)
(require :regexp)
(require :constructor)
(require :mcombin)
(require :uri)

(provide :develenv)
