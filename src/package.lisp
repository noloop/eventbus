(defpackage #:noloop.eventbus
  (:use #:common-lisp)
  (:nicknames #:eventbus)
  (:export #:make-eventbus
	   #:get-listener-count-of-event
           #:once
           #:on))
