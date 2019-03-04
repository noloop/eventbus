(in-package #:noloop.eventbus)

(defun make-eventbus ()
  "Return eventbus instance."
  (make-hash-table :test 'eq))

(defun get-listener-count-of-event (eventbus event-name)
  "Return length listeners of event. Return nil if event nonexistent."
  (length (gethash event-name eventbus)))

(defun get-all-listeners-of-event (eventbus event-name)
  "Return two values, the value: list of listeners of  the event, and present-p: list is present."
  (multiple-value-bind (value present-p)
      (gethash event-name eventbus)
    (values value present-p)))

(defun make-listener (listener-fn is-once)
  "It returns a list, the first element being a listener function, and the following being a boolean saying if it is an once listener."
  (list listener-fn is-once))

(defun once (eventbus event-name listener-fn)
  "Add one listener to an event. The listener is removed when the event is emitted. The add-listener event is emitted before adding the new listener."
  (when (gethash :add-listener eventbus)
    (emit eventbus :add-listener event-name))
  (setf (gethash event-name eventbus)
        (push (make-listener listener-fn t) (gethash event-name eventbus)))
  (values))

(defun on (eventbus event-name listener-fn)
  "Add one listener to an event. The add-listener event is emitted before adding the new listener."
  (when (gethash :add-listener eventbus)
    (emit eventbus :add-listener event-name))
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
    (when (gethash :remove-listener eventbus)
      (emit eventbus :remove-listener event-name))
    (when (eq 0 (get-listener-count-of-event eventbus event-name))
      (remhash event-name eventbus))
    (values)))

(defun emit (eventbus event-name &rest args)
  "Emite an event by passing the arguments offered to the listener function. If the listener is once, then the listener is excluded from the list of listeners."
  (let ((listeners (gethash event-name eventbus)))
        (when listeners
          (dolist (i listeners)
            (let ((fn (car i)))
              (when (cadr i)
                (off eventbus event-name fn))
              (apply fn args))))
    (values)))

(defun get-all-events-name (eventbus)
  "Return one list with all name of events of the eventbus. The list returned includes add-listener and remove-listener."
  (let ((keys '()))
    (maphash
     #'(lambda (key value)
         (declare (ignore value))
         (push key keys))
     eventbus)
    (nreverse keys)))

(defun remove-all-listeners-of-event (eventbus event-name)
  "Removing all listeners from the event. Will be called the off function for each listener, so the remove-listener event is emitted correctly for each listener removed."
  (let* ((listeners (gethash event-name eventbus))
         (listener-fn (car (first listeners))))
    (when listeners
      (off eventbus event-name listener-fn)
      (remove-all-listeners-of-event eventbus event-name))))
