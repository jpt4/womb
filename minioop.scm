;----------------------------------------------------------------------
;;; MINIOOP.SCM

;Date: 8 Feb 92 20:48:07 GMT
;From: Marc Feeley <feeley@zurich.ai.mit.edu>
;Organization: M.I.T. Artificial Intelligence Lab.
;Subject: Minimal OOP support
;Message-Id: <FEELEY.92Feb8154807@zohar.ai.mit.edu>
;To: scheme@mc.lcs.mit.edu

;For those interested in OOP in Scheme, here is a minimal system I have
;used in the past.  It doesn't have any bells and whistles but it is
;simple and fairly portable (you need macros however).

;Marc

; The following code has been corrected and adapted for SCM by
; Guenther Goerz, University of Erlangen-Nuernberg, IMMD 8 (Computer
; Science)
; goerz@informatik.uni-erlangen.de

; Minimal object support for Scheme
;

; Note: This code was originally written for Gambit.
; Modified for SCM by GG (1993)

(defmacro define-method args

  (define (err) (error "Ill-formed `define-method'") #f)

  (define (def-method classes name parms body)
    `(DEFINE ,name
       (MAKE-METHOD ',name
                      (LAMBDA ,parms ,@body)
                      ,@(map (lambda (x) `(LAMBDA () ,x)) classes))))

  (let loop ((args args) (classes '()))
    (if (pair? args)
      (let ((rest (cdr args)) (arg (car args)))
        (cond ((symbol? arg)
               (loop rest (cons arg classes)))
              ((pair? arg)
               (let ((name (car arg)) (parms (cdr arg)))
                 (if (and (pair? classes) (symbol? name) (pair? rest))
                   (def-method classes name parms rest)
                   (err))))
              (else
               (err))))
      (err))))

(define (make-method name proc . classes)
  (let ((method-descr (assq name method-descriptors)))

    (if (not method-descr) ; first definition?

      ; create new method descriptor...

      (let ((method-descr
              (cons name (cons #f (map (lambda (x) (cons x proc)) classes)))))

        (define (generic-proc self . rest)
          (let loop ((l (cddr method-descr)))
            (if (pair? l)
              (let ((entry (car l)))
                (if (((car entry)) self)
                  (apply (cdr entry) self rest)
                  (loop (cdr l))))
              (error "Method is not defined for this object:"
                     (car method-descr) self))))

        (set-car! (cdr method-descr) generic-proc)

        (set! method-descriptors (cons method-descr method-descriptors))

        generic-proc)

      ; update method descriptor if it existed before...

      (let ()

        (define (add-entry class)
          (let ((new-entry (cons class proc)))
            (let loop ((l (cddr method-descr)))
              (let ((entry (car l)) (rest (cdr l)))
                (cond ((eq? (class) ((car entry))) ; replace entry
                       (set-car! l new-entry))
                      ((pair? rest)
                       (loop rest))
                      (else ; add at head of dispatch table
                       (set-cdr! (cdr method-descr)
                         (cons new-entry (cddr method-descr)))))))))

        (for-each add-entry classes)

        (cadr method-descr)))))

; (define methlasses)

(define method-descriptors '())


; `Define-struct' is not strictly necessary for the object system but
; it is useful to define new data types.

(defmacro define-struct (name . fields)

  (define (err) (error "Ill-formed `define-struct'") #f)

  (define (sym . strings) (string->symbol (apply string-append strings)))

  (if (symbol? name)
    (let ((name-str (symbol->string name)))
      (let loop ((l1 fields) (l2 '()) (i 1))
        (if (pair? l1)
          (let ((rest (cdr l1)) (field (car l1)))
            (if (symbol? field)
              (let* ((field-str (symbol->string field))
                     (field-ref (sym name-str "-" field-str))
                     (field-set! (sym name-str "-" field-str "-set!")))
                (loop rest
                      (cons `(DEFINE (,field-set! X Y) (VECTOR-SET! X ,i Y))
                            (cons `(DEFINE (,field-ref X) (VECTOR-REF X ,i))
                                  l2))
                      (+ i 1)))
              (err)))
          `(BEGIN
             ,@l2
             (DEFINE ,(sym "tag-" name-str) (LIST ',name))
             (DEFINE (,(sym "make-" name-str) ,@fields)
               (VECTOR ,(sym "tag-" name-str) ,@fields))
             (DEFINE (,(sym name-str "?") X)
               (AND (VECTOR? X) (= (VECTOR-LENGTH X) ,i)
                    (EQ? (VECTOR-REF X 0) ,(sym "tag-" name-str))))))))
    (err)))

; ------------------------------

; Sample use:  We want to have a `print' procedure that behaves like `write'
; except that numbers are written in base 2, dot notation is used
; for lists and a special format is used for VEHICLE structures.

;  (define (default? x) #t)

;  (define-method default? (print obj)  ;order of definitions is important...
;    (write obj))                       ;search is done from last to first
                                        ;so this is the last to be tested.

;  (define-method number? (print obj)
;    (display "#b")
;    (display (number->string obj 2)))

;  (define-method pair? (print obj)
;    (display "(")
;    (print (car obj))
;    (display " . ")
;    (print (cdr obj))
;    (display ")"))

;  (define-struct vehicle registration-id weight nb-wheels)

;  (define-method vehicle? (print obj)
;    (display "#<VEHICLE id:")
;    (print (vehicle-registration-id obj))
;    (display ">"))

;  (print (list 1 'hello '(2 3) (make-vehicle 14 2000 4)))
; -->
;   (#b1 . (hello . ((#b10 . (#b11 . ())) . (#<VEHICLE id:#b1110> . ()))))

;----------------------------------------------------------------------

