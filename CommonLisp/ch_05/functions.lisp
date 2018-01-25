;;;; Functions and stuff

;;; Defining functions

(defun name (params)
  "Optional documentation string"
    (list params)) ; Body form

(defun function-names-are-kebab-case () ())

(defun foo->bar () ()) ; Converters sometimes use arrows in name

;;; Parameters

;; Optional parameters
(defun foo-optional (a b &optional c d)
  (list a b c d))
; (foo-optional 1 2 )    => (1 2 NIL NIL)
; (foo-optional 1 2 3)   => (1 2 3 NIL)
; (foo-optional 1 2 3 4) => (1 2 3 4)

;; Default values for optional parameters
(defun foo-opt-default (a &optional (b 10))
  (list a b))
; (foo-opt-default 1 2) => (1 2)
; (foo-opt-default 1)   => (1 10)

;; Defaults can use prior parameters
(defun foo-make-rectangle (width &optional (height width))
  (list width height))

;; Parameters can have a "-supplied-p" (convention) variable indicating if it was set
(defun foo-supplied-p (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

;; Rest parameters
(defun foo-rest (&rest params)
  (list params))

;;Keyword parameters
(defun foo-keyword (&key a b c)
  (list a b c))
; (foo-keyword)                => (NIL NIL NIL)
; (foo-keyword :a 1)           => (1 NIL NIL)
; (foo-keyword :b 1)           => (NIL 2 NIL)
; (foo-keyword :c 3)           => (NIL NIL 3)
; (foo-keyword :a 1 :c 3)      => (1 NIL 3)
; (foo-keyword :a 1 :b 2 :c 3) => (1 2 3)
; (foo-keyword :a 1 :c 3 :b 2) => (1 2 3)

;; Keyword parameters can also provide default values and "-supplied-p" variables
(defun foo-key-extra (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))
; (foo-key-extra :a 1)           => (1 0 1 NIL)
; (foo-key-extra :b 1)           => (0 1 1 T)
; (foo-key-extra :b 1 :c 4)      => (0 1 4 T)
; (foo-key-extra :a 2 :b 1 :c 4) => (2 1 4 T)

;; You can even specify the name of the key as being different (this is not super common)
(defun foo-key-name (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
; (foo-key-name :apple 10 :box 20 :charlie 30) => (10 20 30 T)

;;; Combining parameter types
;;; Specify types in this order: required, &optional, &rest, &key

;; Dont combine &optional and &key (in general) e.g.
;; (defun foo-opt-key (x &optional y &key z) (list x y z))
; (foo-opt-key 1 2 :z 3) => (1 2 3)      FINE
; (foo-opt-key 1)        => (1 NIL NIL)  FINE
; (foo-opt-key 1 :z 3)   => ERROR        NOT FINE (:z is taken as value for y, 3 is not a keyword/value pair)
; With two optional params :z and 3 would both be swallowed and there wouldn't be any error!

;; You can combine &rest and &key but they behave slightly oddly
(defun foo-rest-key (&rest rest &key a b c)
  (list rest a b c))
;; Values are bound to both types
; (foo-rest-key :a 1 :b 2 :c 3) => ((:A 1 :B 1 :C 3) 1 2 3)

;;; Returning from functions

;; You can return from inside a function with RETURN-FROM
(defun foo-return (n)
  "Find the first pair of numbers less than 10 whose product is greater than n"
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo-return (list i j))))))

;;; Higher order functions

;; You can get function objects with the function special operator
(defun foo-double (x)
  (* 2 x))
; (function foo-double) => #<FUNCTION FOO-DOUBLE>

;; You can also use #' instead of function
; #'foo-double => #<FUNCTION FOO-DOUBLE>

;; You can call invoke a function with a known number of arguments with FUNCALL
; (foo 1 2 3) === (funcall #'foo 1 2 3)

;; You can take function objects as arguments
(defun foo-plot (fn min max step)
  "Print a historgram of the values return by fn from min to max stepping by step"
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t  "*"))
    (format t "~%")))

;; You can also invoke a function with a list of arguments by using APPLY
(defvar plot-data-1 (list #'foo-double 1 5 1/2))
(apply #'foo-plot plot-data-1) === (foo-plot #'foo-double 1 5 1/2)

;; APPLY can even take loose arguments as long as the last one is a list and the arguments as a whole are valid
(defvar plot-data-2 (list 1 5 1/2))
(apply #'foo-plot #'exp plot-data-2)

;;; Anonymous functions

;; You can define anonymous lambda functions
(lambda (parameters) (list params))

;; (lambda ...) is a macro which expands to #'(lambda ...) so either can be used
; (funcall #'(lambda (x y) (+ x y)) 3 5) => 5
; (funcall (lambda (x y) (+ x y)) 3 5)   => 5
; ((lambda (x y) (+ x y)) 3 5)   => 5
