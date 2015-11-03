(declare (usual-integrations))

;; Root (parent) to all namespaces
(define root-namespace (make-top-level-environment))

;; Keep the list of all namespaces
;; Have a reference to itself
(environment-define 
 root-namespace
 '*namespaces*
 (let ((h (make-strong-eq-hash-table)))
   (hash-table/put! h 'scheme root-namespace)
   h))



