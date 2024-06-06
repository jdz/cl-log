(defpackage #:cl-log.syslog
  (:use #:common-lisp)
  (:import-from #:cl-log
                #:stop-messenger)
  (:export #:*manager*
           #:msg
           #:messenger
           #:start-messenger
           #:stop-messenger))

(in-package #:cl-log.syslog)

(defvar *categories* (make-instance 'cl-log:category-set))

(macrolet ((defcategory (category &optional expands-as)
             `(cl-log:defcategory ,category ,expands-as *categories*)))
  (defcategory :emergency)
  (defcategory :alert (or :alert :emergency))
  (defcategory :critical (or :critical :alert))
  (defcategory :error (or :error :critical))
  (defcategory :warning (or :warning :error))
  (defcategory :notice (or :notice :warning))
  (defcategory :info (or :info :notice))
  (defcategory :debug (or :debug :info)))

(defvar *manager*
  (make-instance 'cl-log:log-manager
                 :categories *categories*))

(defmacro msg (category description &rest args)
  (if (endp args)
      `(cl-log:log-manager-message *manager* ,category "~A" ,description)
      `(cl-log:log-manager-message *manager* ,category ,description ,@args)))

(defclass messenger (cl-log:base-messenger)
  ())

(defun syslog-priority (category)
  (ecase category
    (:emergency 0)
    (:alert     1)
    (:critical  2)
    (:error     3)
    (:warning   4)
    (:notice    5)
    (:info      6)
    (:debug     7)))

(defmethod cl-log:messenger-send-message ((messenger messenger)
                                          (message cl-log:base-message))
  (let ((category (cl-log:message-category message)))
    (sb-posix:syslog (syslog-priority category)
                     (apply #'format nil
                            (cl-log:message-description message)
                            (cl-log:message-arguments message)))))

(defun start-messenger (&rest initargs)
  (apply #'cl-log:start-messenger 'messenger
         :manager *manager*
         initargs))
