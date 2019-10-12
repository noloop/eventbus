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
                #:off
                #:emit
                #:get-all-events-name
                #:remove-all-listeners-of-event))
(in-package #:noloop.eventbus-test)

(defun test-get-all-listeners-of-event ()
  (let* ((eventbus-instance (make-eventbus))
         (fn (lambda ()))
         (actual nil)
         (expected (list (list fn t)))
         (existent-p nil))
    (setf (gethash :event-1 eventbus-instance) (list (list fn t)))
    (multiple-value-bind (value present-p)
        (get-all-listeners-of-event eventbus-instance :event-1)
      (setf actual value)
      (setf existent-p present-p))
    (and (equal actual expected)
         existent-p)))

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

(defun test-emit-once-event ()
  (let ((eventbus-instance (make-eventbus))
        (x 0)
        (expected 1))
    (once eventbus-instance :event-1 (lambda () (incf x)))
    (emit eventbus-instance :event-1)
    (emit eventbus-instance :event-1)
    (eql x expected)))

(defun test-emit-on-event ()
  (let ((eventbus-instance (make-eventbus))
        (x 0)
        (expected 2))
    (on eventbus-instance :event-1 (lambda () (incf x)))
    (emit eventbus-instance :event-1)
    (emit eventbus-instance :event-1)
    (eql x expected)))

(defun test-emit-on-event-with-args ()
  (let ((eventbus-instance (make-eventbus))
        (x 0)
        (expected 8))
    (on eventbus-instance :event-1 (lambda (n z) (incf x (+ n z))))
    (emit eventbus-instance :event-1 2 2)
    (emit eventbus-instance :event-1 2 2)
    (eql x expected)))

(defun test-add-listener-emit-once-event ()
  (let ((eventbus-instance (make-eventbus))
        (actual nil)
        (expected :event-1))
    (once eventbus-instance :add-listener
          (lambda (event-name) (setf actual event-name)))
    (on eventbus-instance :event-1 (lambda ()))
    (eql actual expected)))

(defun test-add-listener-emit-on-event ()
  (let ((eventbus-instance (make-eventbus))
        (actual nil)
        (expected :event-1))
    (on eventbus-instance :add-listener
        (lambda (event-name) (setf actual event-name)))
    (on eventbus-instance :event-1 (lambda ()))
    (eql actual expected)))

(defun test-remove-listener-emit-on-event ()
  (let* ((eventbus-instance (make-eventbus))
         (actual nil)
         (expected :event-1)
         (listener-fn
           (lambda (event-name) (setf actual event-name))))
    (on eventbus-instance :remove-listener listener-fn)
    (off eventbus-instance :event-1 listener-fn)
    (eql actual expected)))

(defun test-get-all-events-name ()
  (let ((eventbus-instance (make-eventbus))
        (actual nil)
        (expected '(:remove-listener :event-once-1 :event-on-1)))
    (on eventbus-instance :remove-listener (lambda ()))
    (on eventbus-instance :event-once-1 (lambda ()))
    (on eventbus-instance :event-on-1 (lambda ()))
    (setf actual (get-all-events-name eventbus-instance))
    (equal actual expected)))

(defun test-remove-all-listeners-of-event ()
  (let ((eventbus-instance (make-eventbus))
        (actual nil)
        (expected 0))
    (on eventbus-instance :event-1 (lambda ()))
    (on eventbus-instance :event-1 (lambda ()))
    (remove-all-listeners-of-event eventbus-instance :event-1)
    (setf actual
          (get-listener-count-of-event eventbus-instance :event-1))
    (eql actual expected)))

(defun test-remove-all-listeners-of-event-with-event-name-remove-listener ()
  (let ((eventbus-instance (make-eventbus))
        (actual nil)
        (expected 0)
        (listener-fn
          (lambda (event-name) event-name)))
    (on eventbus-instance :remove-listener listener-fn)
    (on eventbus-instance :remove-listener listener-fn)
    (remove-all-listeners-of-event eventbus-instance :remove-listener)
    (setf actual
          (get-listener-count-of-event eventbus-instance :remove-listener))
    (eql actual expected)))

(defun test-remove-all-listeners-of-event-with-event-name-add-listener ()
  (let ((eventbus-instance (make-eventbus))
        (actual nil)
        (expected 0)
        (listener-fn
          (lambda (event-name) event-name)))
    (on eventbus-instance :add-listener listener-fn)
    (on eventbus-instance :add-listener listener-fn)
    (remove-all-listeners-of-event eventbus-instance :add-listener)
    (setf actual
          (get-listener-count-of-event eventbus-instance :add-listener))
    (eql actual expected)))

(suite "Suite eventbus"
       (test "Test get-all-listeners-of-event" #'test-get-all-listeners-of-event)
       (test "Test get-listener-count-of-event" #'test-get-listener-count-of-event)
       (test "Test once" #'test-once)
       (test "Test on" #'test-on)
       (test "Test off" #'test-off)
       (test "Test off-remove-all-listeners-of-event" #'test-off-remove-all-listeners-of-event)
       (test "Test emit-once-event" #'test-emit-once-event)
       (test "Test emit-on-event" #'test-emit-on-event)
       (test "Test emit-on-event-with-args" #'test-emit-on-event-with-args)
       (test "Test add-listener-emit-once-event" #'test-add-listener-emit-once-event)
       (test "Test add-listener-emit-on-event" #'test-add-listener-emit-on-event)
       (test "Test remove-listener-emit-on-event" #'test-remove-listener-emit-on-event)
       (test "Test get-all-events-name" #'test-get-all-events-name)
       (test "Test remove-all-listeners-of-event" #'test-remove-all-listeners-of-event)
       (test "Test remove-all-listeners-of-event-with-event-name-remove-listener" #'test-remove-all-listeners-of-event-with-event-name-remove-listener)
       (test "Test remove-all-listeners-of-event-with-event-name-add-listener" #'test-remove-all-listeners-of-event-with-event-name-add-listener))
