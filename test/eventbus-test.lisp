(defpackage #:noloop.eventbus-test
  (:use #:common-lisp
        #:simplet)
  (:nicknames #:eventbus-test)
  (:import-from #:eventbus
                #:make-eventbus
                #:get-all-listeners-of-event
                #:get-listener-count-of-event
                #:once
                #:on
                #:off))
(in-package #:noloop.eventbus-test)

(defun test-get-all-listeners-of-event ()
  (let ((eventbus-instance (make-eventbus))
        (actual nil)
        (existent-p nil))
    (setf (gethash :event-1 eventbus-instance) (list (list (lambda ()) t)))
    (multiple-value-bind (value present-p)
        (get-all-listeners-of-event eventbus-instance :event-1)
      (setf actual value)
      (setf existent-p present-p))
    (and actual existent-p)))

(defun test-get-listener-count-of-event ()
  (let ((eventbus-instance (make-eventbus))
        (expected 1)
        (actual nil))
    (setf (gethash :event-1 eventbus-instance) (list (list (lambda ()) t)))
    (setf actual (get-listener-count-of-event eventbus-instance :event-1))
    (= expected actual)))

(defun test-once ()
  (let ((eventbus-instance (make-eventbus))
        (expected 1)
        (actual nil))
    (once eventbus-instance :event-once-1
          (lambda ()
            ;; make something at emit this event.
            ))
    (setf actual (get-listener-count-of-event eventbus-instance :event-once-1))
    (= expected actual)))

(defun test-on ()
  (let ((eventbus-instance (make-eventbus))
        (expected 1)
        (actual nil))
    (on eventbus-instance :event-on-1
          (lambda ()
            ;; make something at emit this event.
            ))
    (setf actual (get-listener-count-of-event eventbus-instance :event-on-1))
    (= expected actual)))

(defun test-off ()
  (let ((eventbus-instance (make-eventbus))
        (listener-fn-1 (lambda ()))
        (expected 1)
        (actual nil))
    (once eventbus-instance :event-1 listener-fn-1)
    (on eventbus-instance :event-1 (lambda ()))
    (off eventbus-instance :event-1 listener-fn-1)
    (setf actual (get-listener-count-of-event eventbus-instance :event-1))
    (= expected actual)))

(defun test-off-remove-all-listeners-of-event ()
  (let ((eventbus-instance (make-eventbus))
        (listener-fn-1 (lambda ()))
        (actual nil)
        (existent-p t))
    (once eventbus-instance :event-1 listener-fn-1)
    (on eventbus-instance :event-1 listener-fn-1)
    (off eventbus-instance :event-1 listener-fn-1)
    (off eventbus-instance :event-1 listener-fn-1)
    (multiple-value-bind (value present-p)
        (get-all-listeners-of-event eventbus-instance :event-1)
      (setf actual value)
      (setf existent-p present-p))
    (and (null actual)
         (null existent-p))))

(suite "Suite eventbus"
       (test "Test get-all-listeners-of-event" #'test-get-all-listeners-of-event)
       (test "Test get-listener-count-of-event" #'test-get-listener-count-of-event)
       (test "Test once" #'test-once)
       (test "Test on" #'test-on)
       (test "Test off" #'test-off)
       (test "Test off-remove-all-listeners-of-event" #'test-off-remove-all-listeners-of-event)
       )
;; (format t "actual: ~a ,existent-p: ~a~%" actual existent-p)
