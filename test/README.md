# eventbus Test

## How to test eventbus?

It's quite simple, in REPL:

```lisp
(asdf:test-system :eventbus)
```

If "T" is returned in Test result because it passed, if "nil" is returned because it failed.
