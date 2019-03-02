(in-package #:noloop.eventbus)

(defun make-eventbus ()
  (make-hash-table :test 'eq))

(defun get-listener-count-of-event (eventbus event-name)
  "Return length listeners of event. Return nil if event nonexistent."
  (length (gethash event-name eventbus)))

(defun get-all-listeners-of-event (eventbus event-name)
  (multiple-value-bind (value present-p)
      (gethash event-name eventbus)
    (values value present-p)))

(defun make-listener (listener-fn is-once)
  "It returns a list, the first element being a listener function, and the following being a boolean saying if it is an once listener."
  (list listener-fn is-once))

(defun once (eventbus event-name listener-fn)
  "Add one listener to an event. The listener is removed when the event is emitted."
  (setf (gethash event-name eventbus)
        (push (make-listener listener-fn t) (gethash event-name eventbus)))
  (values))

(defun on (eventbus event-name listener-fn)
  "Add one listener to an event."
  (setf (gethash event-name eventbus)
        (push (make-listener listener-fn nil) (gethash event-name eventbus)))
  (values))

(defun off (eventbus event-name listener-fn)
  "Remove the first listener from the event listeners list."
  (let ((listeners (gethash event-name eventbus)))
    (setf (gethash event-name eventbus)
          (delete-if #'(lambda (i)
                         (eq (car i) listener-fn))
                     listeners
                     :count 1))
    (when (eq 0 (get-listener-count-of-event eventbus event-name))
      (remhash event-name eventbus))
    (values)))
