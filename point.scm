; Representing objects in Scheme
; and some interesting ramifications
; Soumen Chakrabarti 2003/10/30

; The most common thing we do with an object is to invoke a method
; associated with it, in the form obj.meth(args...) in C++ or Java.
; This is implemented easily in Scheme, by making the object a lambda
; whose param is a method name.  E.g., suppose we want to define a 1d
; point object with a variable x inside it and two methods get and put.

(define point0-object
  (let ((x 0))
    (lambda (method-name)
      (cond
       ((eq? method-name 'get) x)
       ((eq? method-name 'put) (lambda (newx) (set! x newx)))
       (else 'unknown-method) ) ) ) )

; Observe how the let hides x from outside view, except through the
; access methods get and put.  This is called encapsulation in OO
; jargon.  Given this object, we can invoke methods as follows:

;]=> (  point0-object 'get )
;]=> ( (point0-object 'put) 1 )
;]=> (  point0-object 'get )

; In general, we would like to create many point0 objects, and it
; would be inconvenient to write out the same definitions of put and
; get again and again.  To avoid this, we can create a "point factory"
; --- a function which can be called to return a fresh point object.
; In OO terms, such a "factory" is called a "class".  A class is also
; an object.  It has only one method called "new".

(define point0-class
  (lambda (method-name)
    (cond
     ((eq? method-name 'new)
      ; here we stick in a point object
      (let ((x 0))
	(lambda (method-name)
	  (cond
	   ((eq? method-name 'get) x)
	   ((eq? method-name 'put) (lambda (newx) (set! x newx)))
	   (else 'unknown-method) ) ) )
      ; end of insert
      )
     (else 'unknown-method) ) ) )

; We can now create a new point object (called an "instance" of the
; "class" in OO jargon) by

(define point0-instance-1 (point0-class 'new))
( (point0-instance-1 'put) 2 )
(define point0-instance-2 (point0-class 'new))
( (point0-instance-2 'put) 3 )

; Now try
;]=> (+ (point0-instance-1 'get) (point0-instance-2 'get) )
;Value: 5

; This shows that these two objects have independent existence with
; separate mutable cells for their individual x values.

; Next we will study inheritance ("extends" in Java).  We start by
; extending an instance rather than a class.  Suppose we want an
; object like point0-instance-1, except that the method "get" should
; return x+1, not x.

(define point1-instance-1
  (let ((base-instance (point0-class 'new) ) )
    (lambda (method-name)
      (cond
       ((eq? method-name 'get) (1+ (base-instance 'get)  ) )
	; we first check the current object, then relegate to
        ; the base instance
       (else ( base-instance method-name ) ) ) ) ) )

; Note that we wrote (1+ (base-instance 'get)), because we can't see x
; unless we look inside base-instance.  We will come back to this
; later.

; Try:
;]=> ( (point1-instance-1 'put) 4 )
;]=> (  point1-instance-1 'get )
;Value: 5
; as expected, not 4.

; Another important feature related to inheritance is the ability to
; access the base-instance from outside.  This is done in C++ and Java
; by type-casting, as shown below.
;    // C++                          // Java
;    Ext *e = new Ext();             Ext e = new Ext();
;    Base *b = (Base*) e;            Base b = (Base) e;
; We can implement this in our style by adding a new method "super"
; which returns the base instance embedded in the extended instance.

(define point1-instance-2
  (let ((base-instance (point0-class 'new) ) )
    (lambda (method-name)
      (cond
       ((eq? method-name 'get) (1+ (base-instance 'get)  ) )
       ((eq? method-name 'super) base-instance)
       (else ( base-instance method-name ) ) ) ) ) )

( (point1-instance-2 'put) 6 )
(define base1-instance-2 (point1-instance-2 'super))
;Try
;]=> (base1-instance-2 'get)
;Value: 6
;]=> ( (point1-instance-2 'put) 7 )
;]=> (base1-instance-2 'get)
;Value: 7

; So what you are doing to the extended object is affecting the
; results of methods called on the base object, but note that the
; "get" in base-instance-2 is back to reporting x, not x+1.

; However, this is NOT how Java works.  Check out the code
; Inherit.java.  Effectively, if a base instance is obtained by
; calling the "super" method in an extended instance, the "get" method
; called when we say (base-instance 'get) is the one in the *extended*
; instance.

; Why is this a desirable feature?  Here is a realistic scenario.

; <scenario>
; Various Task objects flow around in a workflow management system.
; Semantically, a Task may be "get key perms for a lab" or "get
; replacement batteries for the UPS".  Tasks pass through multiple
; agents (lab-in-charge, HoD, vendor, Director) before completion,
; may pass through an agent many times ("price is too high"), and is
; in general completed by an agent different from the Task creator.

; Suppose I am interested in logging turnaround-time statistics from
; the creation to completion of my tasks.  So I extend Task to MyTask
; and override the start() and complete() methods.  The MyTask.start
; records the clock (in addition to doing what Task.start does), and
; MyTask.complete reads the clock, subtracts, and sends me email
; (before doing what Task.complete does).

; No one else in the system knows, or needs to know, that I have
; released an extension of Task into the system.  They can still call
; all other standard methods in Task on my object.  As long as I give
; the system an Object which can be cast to a Task, everyone is ok.

; However, if casting to Task means my_instance.start() and
; my_instance.complete() lose their extended meanings, I lose.
; </scenario>

; Java is said to implement "virtual methods" in extended objects.
; C++ gives you an option.  If you want a method to be virtual, you
; have to use a "virtual" keyword, see Inherit.cpp in this directory.

; Our next step is to implement Java-like compulsory virtual methods
; in our Scheme style.  What should the "super" method do if we want
; virtual methods?  Clearly, it should not just return the base
; instance, or the extended methods will be forgotten.

(define point1-instance-3
  (let ((base-instance (point0-class 'new) ) )
    (lambda (method-name)
      (cond
       ((eq? method-name 'super) 
	; can't just return base-instance
	(lambda (method-name)
	  ; Dangerous!  This test may have unwanted side effects.
	  (if (eq? (base-instance method-name) 'unknown-method)
	      'unknown-method
	      ; else deal with method-name myself
	      (cond
	       ((eq? method-name 'get) (1+ (base-instance 'get)))
	       (else ( base-instance method-name ) )   )
	      ; this is redundant
	  ) ) )
       ((eq? method-name 'get) (1+ (base-instance 'get)  )  )
       (else ( base-instance method-name ) )
       );cond
      );outer dispatcher
    );let base
  );define

( (point1-instance-3 'put) 8 )
(define base-instance-3 (point1-instance-3 'super))

;Try
;]=> (base-instance-3 'get)
;Value: 9
;]=> ( (point1-instance-2 'put) 9 )
;]=> (base-instance-2 'get)
;Value: 10

; How do we remove the redundancy?  Somehow, the method dispatch table
; must be "aware of itself", which is why we need the "this" parameter.
; First we define a small macro for convenience.

(define-syntax mcall ; method-call
  (syntax-rules ()
    ((mcall Eo Em) ( (Eo Em) Eo ) ) ) )

; Now we change our method motif to have a compulsory first param "this".

(define point2-instance-1
  (let ((x 0))
    (lambda (method-name)
      (cond
       ((eq? method-name 'get) (lambda (this) x))
       ((eq? method-name 'put) (lambda (this)
				 (lambda (newx) (set! x newx)) ) )
       ; What if we want one method to call another?  Once we can access
       ; "this", we can easily support methods calling other methods
       ; (or even themselves).
       ((eq? method-name 'inc) (lambda (this)
				 ( (mcall this 'put)
				   (1+ (mcall this 'get)) ) );lambda
	)
       (else 'unknown-method) );cond
      );dispatch
    );let
  )

; ]=> ((mcall point2-instance-1 'put) 3)
;Value: 0
; ]=> (mcall point2-instance-1 'get)
;Value: 3
; ]=> (mcall point2-instance-1 'inc)
;Value: 3
; ]=> (mcall point2-instance-1 'get)
;Value: 4

; Now let us see if the "this" motif helps us remove the redundant
; method definitions in point1-instance-3.

(define point3-instance-1
  (let ((base-instance point2-instance-1))
    (lambda (method-name)
      (cond
       ((eq? method-name 'get) ; override 'get to return x+1
	(lambda (this) (1+ (mcall base-instance 'get))) )
					; Again, note we don't see the
					; name "x" in this lexical scope.

       ; Methods in the extended instance can use methods in the base 
       ; instance without even knowing they are in the base instance.
       ((eq? method-name 'inc2)
	(lambda (this) (begin (mcall this 'inc)
			      (mcall this 'inc) ) )  )

       ((eq? method-name 'super)
	(lambda (this)
	  ; return must be an object
	  (lambda (inner-method)
					; Unlike the previous "this"-less
					; implementation, here we are in 
					; no danger of a side effect if we
					; call (base-instance inner-method)
					; because the result is always a
					; (lambda (obj) ... )
	    (if (eq? (base-instance inner-method) 'unknown-method)
		'unknown-method
		(this inner-method)  ; NOTE: no use of mcall here
		);if
	    );inner-dispatch
	  ; end of return object
	);this
	);eq
       (else (base-instance method-name) )  ; NOTE: no mcall here either
       );cond
      );dispatch
    );let
  )

( (mcall point3-instance-1 'put) 100 )

; The last goal is to support both virtual and non-virtual methods in
; a single object.  We would like to declare these two kinds of
; methods using syntax like this:
;    (method  M (Ia*) Em)
;    (virtual M (Ia*) Em)
; A uniform (mcall Eo M) should dispatch to the correct M in a base
; or extended instance according as the method was declared.

; (virtual M ...) results in two dispatch stanzas:
;   ((eq? method-name 'M) ...)
;   ((eq? method-name 'M-is-virtual) #t)
; (method  M ...) results in two dispatch stanzas:
;   ((eq? method-name 'M) ...)
;   ((eq? method-name 'M-is-virtual) #f)

; Some hand translations:

(define base-instance-1
  (let ((x 0))
    (lambda (method-name)
      (cond
       ; get stanza
       ((eq? method-name 'get) (lambda (this) x))
       ((eq? method-name 'get-is-virtual) #t)
					; NOTE: try both #t and #f and
					; see the effect
       ; put stanza
       ((eq? method-name 'put) (lambda (this) (lambda (newx) (set! x newx))))
       ((eq? method-name 'put-is-virtual) #f)
       (else 'unknown-method)  );cond	
      );dispatch
    );member
  )

; a small helper function
(define (symbol-concat s1 s2)
  (string->symbol (string-append (symbol->string s1)
				 (symbol->string s2) ) ) )

(define ext-instance-1
  (let ((base-instance base-instance-1))
    (lambda (method-name)
      (cond
       ; super stanza --------------------------------------------------
       ((eq? method-name 'super)
	(lambda (this)
	  (lambda (inner)
	    (if (eq? (base-instance inner) 'unknown-method)
		'unknown-method
		; now test inner-method-is-virtual
		(if (base-instance (symbol-concat inner '-is-virtual))
		    (this inner)
		    (base-instance inner) ); virtual or not		
		);if method defined in base instance
	    )
	  );lambda inner dispatch
	);super method
       ((eq? method-name 'super-is-virtual) #f)	; super is never virtual
       ;----------------------------------------------------------------
       ; inc stanza
       ((eq? method-name 'inc)
	(lambda (this) ((mcall this 'put) (1+ (mcall this 'get) ) ) ) )
       ((eq? method-name 'inc-is-virtual) #f) ; don't want to extend inc
       ;----------------------------------------------------------------
       ; get stanza ... override get to return x+1 instead of x
       ((eq? method-name 'get)
	(lambda (this) (1+ (mcall base-instance 'get))) )
       ((eq? method-name 'get-is-virtual) #f) ; forbid further extension
       ;----------------------------------------------------------------
       ; default stanza to relegate to base instance
       (else (base-instance method-name))  );cond
      );dispatch
    );member
  )

; ]=> (mcall ext-instance-1 'get)
;Value: 1
; ]=> (mcall ext-instance-1 'inc)
;Value: 0
; ]=> (mcall ext-instance-1 'get)
;Value: 3
; ]=> (define cast-to-base (mcall ext-instance-1 'super))
;Value: cast-to-base
; ]=> (mcall cast-to-base 'get)
;Value: 2

; Discussions:
; * Each member y leads to system-generated non-virtual methods
;   _get_y and _put_y
; * Accesses to y by an extending instance are replaced by mcalls to
;   _get_y and _put_y
; * Once you use "this", a method calling itself and methods calling
;   each other in mutual recursion, are no big deal.
