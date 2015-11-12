(declare (usual-integrations))

;; Root (parent) to all namespaces
;; (define root-namespace (make-top-level-environment))
;; for scmutils: different parent environment

(define root-environment (extend-top-level-environment generic-environment))

;; Keep the list of all namespaces
;; Have a reference to itself
(environment-define 
 root-environment
 '*namespaces*
 (let ((h (make-strong-eq-hash-table)))
   (hash-table/put! h 'scheme (cons root-environment
				    (make-strong-eq-hash-table)))
   h))


#|
(let ((r root-environment)
      (p (->environment '(package))))
  (link-variables r 'make-raw-namespace p 'make-package)
  (link-variables r 'namespace? p 'package?)
  (link-variables r 'list-namespaces p 'all-packages))
|#  


#|
(define root-namespace (extend-top-level-environment generic-environment))




|#
