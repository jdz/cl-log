;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-log/cl-log.1.0.1/cl-log.asd#1 $

(in-package asdf)

(defsystem :cl-log
    :description "CL-LOG - a general purpose logging utility"
    :version "1.0.1"
    :author "Nick Levine <ndl@ravenbrook.com>"
    :licence "Public Domain"
    :components ((:file "pkg")
                 (:file "cl-log" :depends-on ("pkg"))))


(defsystem "cl-log/syslog"
  :description "syslog distination for cl-log"
  :author "Jānis Džeriņš <lisp@jonis.lv>"
  :license "Zlib"
  :depends-on ("cl-log")
  :components ((:file "syslog")))
