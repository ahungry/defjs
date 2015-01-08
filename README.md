defjs
======

Common Lisp REPL results in your browser via Javascript!

To try it out in your REPL you can use (ql:quickload :defjs)
if you have added to your ASDF load path in local projects.

## Getting started

### Clone to your local projects directory (until it gets in quicklisp)

```shell
git clone https://github.com/ahungry/defjs.git
```

Then make sure quicklisp can see it:
```lisp
(pushnew #P"/path/you/cloned/to" ql:*local-project-directories*)
(ql:quickload :defjs)
```

And next choose to use it (include in your .asd or directly) or prefix with the package name on each call below:
```lisp
(use-package :defjs)
```

Now visit: http://localhost:39998 - if you see text telling you a small test sample to do,
feel free to try it out and check it out.

## Examples

### Setting up a new function (it will load dynamically in browser on compile and persist on new page refresh as well)

First, define the javascript function:
```lisp
(defjs hello-name (name)
  (alert (+ "Hello " name)))
```
Then, to call it from SLIME:
```lisp
(dojs (hello-name "Matt"))
```

At which point, you'll see "Hello Matt" pop up in your web browser.
