# eventbus

### _An event bus in Common Lisp._

## Getting Started in eventbus

### Download and installation

**1 - Download eventbus system**

By quicklisp:

```
IN PROGRESS...
```

or directly from github:

```
git clone https://github.com/noloop/eventbus.git
```
**2 - Install eventbus**

By quicklisp:

```
IN PROGRESS...
```

or directly from asdf:

```lisp
(asdf:load-system :eventbus)
```

_**Note: Remember to configure asdf to find your directory where you downloaded the libraries (asdf call them "systems") above, if you do not know how to make a read at: https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html or https://lisp-lang.org/learn/writing-libraries.**_

## Create eventbus instance

```lisp
(let ((eventbus-instance (make-eventbus)))
  ;; ...
  )
```

## Add listener in event once

An once event is an event that can only be emitted once because the added listener is deleted after it is first emitted.

```lisp
(let ((eventbus-instance (make-eventbus)))
  (once eventbus-instance :my-event-name
        (lambda ()
          ;;do something
          )))
```
## Add listener in event on

An once event is an event that can only be emitted once because the added listener is deleted after it is first emitted.

```lisp
(let ((eventbus-instance (make-eventbus)))
  (on eventbus-instance :my-event-name
      (lambda ()
        ;;do something
        )))
```

You can also pass arguments to the listener function, both in `once` and `on`. See example:

```lisp
(let ((eventbus-instance (make-eventbus)))
  (once eventbus-instance :my-event-name
        (lambda (arg1 arg2 arg3)
          ;;do something with arg1 arg2 arg3
          ))
  (on eventbus-instance :my-event-name
      (lambda (arg1 arg2 arg3)
        ;;do something with arg1 arg2 arg3
        )))
```

## Remove listener of event

You can remove listeners that have been added, both in `once` and `on`.

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda () t)))
  (on eventbus-instance :my-event-name listener-fn)
  (off eventbus-instance :my-event-name listener-fn))
```

## Emit event

When emitting events, it is possible to pass arguments that will be passed to the listener function.

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda (a b c) '(a b c))))
  (on eventbus-instance :my-event-name listener-fn)
  (emit eventbus-instance :my-event-name 1 2 3))
```

## Get all event names from an instance of eventbus

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda () t)))
  (on eventbus-instance :my-event-name listener-fn)
  (get-all-events-name eventbus-instance))
```

## Get listener count of event

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda () t)))
  (on eventbus-instance :my-event-name listener-fn)
  (get-listener-count-of-event eventbus-instance :my-event-name))
```

## Get all listeners from event

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda () t)))
  (on eventbus-instance :my-event-name listener-fn)
  (get-all-listeners-of-event eventbus-instance :my-event-name))
```

## Get all event names

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda () t)))
  (on eventbus-instance :my-event-name listener-fn)
  (get-all-events-name eventbus-instance))
```

## Remove all listeners of event

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda () t)))
  (once eventbus-instance :my-event-name listener-fn)
  (on eventbus-instance :my-event-name listener-fn)
  (on eventbus-instance :my-event-name listener-fn)
  (remove-all-listeners-of-event eventbus-instance :my-event-name))
```

## Events :add-listener and :remove-listener

There are two standard eventbus events, `:add-listener` is emitted when a new listener is added to an event, while `:remove-listener` is emitted when a listener is removed from an event.

```lisp
(let ((eventbus-instance (make-eventbus))
      (listener-fn (lambda () t)))
  (on eventbus-instance :add-listener
      (lambda ()
        ;; do something when emitted :add-listener event
        ))
  (on eventbus-instance :remove-listener
      (lambda ()
        ;; do something when emitted :remove-listener event
        ))
  (on eventbus-instance :my-event-name listener-fn)
  (off eventbus-instance :my-event-name listener-fn))
```

## API

function **(make-eventbus)**

function **(get-listener-count-of-event eventbus event-name)**

function **(get-all-listeners-of-event eventbus event-name)**

function **(once eventbus event-name listener-fn)**

function **(on eventbus event-name listener-fn)**

function **(off eventbus event-name listener-fn)**

function **(emit eventbus event-name &rest args)**

function **(get-all-events-name  eventbus)**

function **(remove-all-listeners-of-event  eventbus event-name)**

event **:add-listener**

event **:remove-listener**