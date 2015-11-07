(declare (usual-integrations))

(define (sub1 n) (-1+ n))

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

(define-syntax when
  (syntax-rules ()
    ((when ?text ?expr ...)
     (if ?text (begin ?expr ...)))))

