;;; --------------------------------------------------------------------------
;;;
;;;     _/
;;;    _/    _/_/      _/_/_/    _/_/_/    _/_/    _/  _/_/
;;;   _/  _/    _/  _/    _/  _/    _/  _/_/_/_/  _/_/
;;;  _/  _/    _/  _/    _/  _/    _/  _/        _/
;;; _/    _/_/      _/_/_/    _/_/_/    _/_/_/  _/
;;;                    _/        _/
;;;                 _/_/      _/_/
;;;
;;; MODULE: logger.lisp
;;;
;;; @package ostrich-ideas
;;; @author  Dennis Drown
;;; @date    26 Jan 2013
;;;
;;; @copyright 2016 Dennis Drown and  Ostrich Ideas
;;;
;;; --------------------------------------------------------------------------
(defpackage :ostrich-ideas
  (:use   :common-lisp)
  (:nicknames :oi)
  (:export #:log-set-wow
           #:log-msg
           #:log-panic
           #:log-crit
           #:log-error
           #:log-warn
           #:log-notice
           #:log-info
           #:log-debug))

(in-package :ostrich-ideas)

(defparameter *LOG-BLUE*    (format nil "~a[1;34m" #\Esc))
(defparameter *LOG-ORANGE*  (format nil "~a[1;33m" #\Esc))
(defparameter *LOG-RED*     (format nil "~a[0;31m" #\Esc))
(defparameter *LOG-TEXT*    (format nil "~a[0m"    #\Esc))

;; ---------------------------------------------------------------------------
(defvar *LOGGER*    t       "Stream for logging")
(defvar *LOG-WOW*   t       "Make it fancy when logging to standard output")


;; ---------------------------------------------------------------------------
;; ╻  ┏━┓┏━╸   ┏━┓┏━╸╺┳╸   ╻ ╻┏━┓╻ ╻
;; ┃  ┃ ┃┃╺┓╺━╸┗━┓┣╸  ┃ ╺━╸┃╻┃┃ ┃┃╻┃
;; ┗━╸┗━┛┗━┛   ┗━┛┗━╸ ╹    ┗┻┛┗━┛┗┻┛
(declaim (inline log-set-wow))
;; ---------------------------------------------------------------------------
(defun log-set-wow (&optional (yesno t))
"
Set whether or not to use colours and generally a fancy style when logging  to
standard output.  Specify YESNO as t or 'WOW for fancy logging (the default),
or as NIL (or anything else) for plain monochromatic text logging.
"
  (if (eq yesno t)
      (prog1 :| Wow!!! | (setq *LOG-WOW* t))
      (prog1 :| Aww... | (setq *LOG-WOW* nil))))


;; ---------------------------------------------------------------------------
;; ╻ ╻┏━┓╻ ╻
;; ┃╻┃┃ ┃┃╻┃
;; ┗┻┛┗━┛┗┻┛
;; ---------------------------------------------------------------------------
(defun wow (level)
  (let ((colour (when (and *LOG-WOW* (eq *LOGGER* t))
                  (cond
                    ((member level '(PANIC CRIT ERROR)) *LOG-RED*)
                    ((eq     level 'WARN)               *LOG-ORANGE*)
                    ((eq     level 'NOTICE)             *LOG-BLUE*)))))
    (if (null colour)
        level
        (format nil  "~A~A~A" colour level *LOG-TEXT*))))


;; ---------------------------------------------------------------------------
(defmacro log-msg (level &rest msg-args)
"General logger function.  Prepends date-time stamp and informational level"

  `(multiple-value-bind (sec min hour day month year) (get-decoded-time)
     (format *LOGGER* "~&~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d: ~a: "
                      year month day
                      hour min   sec
                      (wow ,level))
     (format *LOGGER* ,@msg-args)))

(defmacro log-panic  (&rest msg-args) `(log-msg 'PANIC  ,@msg-args))
(defmacro log-crit   (&rest msg-args) `(log-msg 'CRIT   ,@msg-args))
(defmacro log-error  (&rest msg-args) `(log-msg 'ERROR  ,@msg-args))
(defmacro log-warn   (&rest msg-args) `(log-msg 'WARN   ,@msg-args))
(defmacro log-notice (&rest msg-args) `(log-msg 'NOTICE ,@msg-args))
(defmacro log-info   (&rest msg-args) `(log-msg 'INFO   ,@msg-args))
(defmacro log-debug  (&rest msg-args) `(log-msg 'DEBUG  ,@msg-args))
