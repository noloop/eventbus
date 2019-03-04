(defpackage #:noloop.eventbus
  (:use #:common-lisp)
  (:nicknames #:eventbus)
  (:export #:make-eventbus
           #:get-all-listeners-of-event
	   #:get-listener-count-of-event
           #:once
           #:on
           #:off
           #:emit
           #:get-all-events-name
           #:remove-all-listeners-of-event))
