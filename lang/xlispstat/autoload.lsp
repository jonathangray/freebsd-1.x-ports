;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.

(provide "autoload")

(defmacro autoload (name module)
  `(if (not (member ,module *modules*))
       (defun ,name (&rest args)
         (fmakunbound ',name)
         (require ,module)
         (apply ',name args))))
    
(autoload nreg-model "nonlin")
(autoload oneway-model "oneway")
(autoload newtonmax "maximize")
(autoload nelmeadmax "maximize")
(autoload bayes-model "bayes")
(autoload step "step")
