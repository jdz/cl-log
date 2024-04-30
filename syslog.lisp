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

;;; It only makes sense to have one of these -- we cannot have multiple
;;; instances of SYSLOG-MESSENGER with different `ident', `facility' or
;;; `option' values.  Currently only `ident' can be specified.
(defclass messenger (cl-log:base-messenger)
  ((ident
    :initarg :ident
    :reader syslog-messenger-ident)))

(defvar %syslog-messenger nil)

(defmethod initialize-instance :after ((instance messenger)
                                       &key ident)
  (let ((old-messenger %syslog-messenger))
    (cond (old-messenger
           (let ((old-ident (syslog-messenger-ident old-messenger))
                 (class (class-name old-messenger)))
             (if (equal ident old-ident)
                 (warn "~A already started." class)
                 (warn "~A with conflicting IDENT (~A) already started."
                       class old-ident))))
          (ident
           ;; XXX: Options are hard coded, default facility (LOG_USER) is used.
           (sb-posix:openlog ident sb-posix:log-pid))
          (t
           (error "Cannot create SYSLOG-MESSENGER without :IDENT.")))))

;;; Best-effort own foot shooting prevention.
(defclass closed-syslog-messenger (messenger)
  ())

(defmethod cl-log:stop-messenger :after ((messenger messenger) &key)
  (change-class messenger 'closed-syslog-messenger)
  (sb-posix:closelog)
  (setf %syslog-messenger nil))

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

(defmethod cl-log:messenger-send-message ((messenger closed-syslog-messenger)
                                          (message t))
  (error "Cannot log message - syslog closed."))

(defun start-messenger (&rest initargs)
  (apply #'cl-log:start-messenger 'messenger
         :manager *manager*
         initargs))
