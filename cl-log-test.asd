;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-log/cl-log.1.0.1/cl-log-test.asd#1 $

(in-package asdf)

(defsystem :cl-log-test
    :depends-on (:cl-log :eos)
    :description "Test suite for CL-LOG - the general purpose logging utility"
    :version "0.1.0"
    :author "Nick Barnes <nb@ravenbrook.com>"
    :licence "Public Domain"
    :components ((:file "cl-log-test")))

