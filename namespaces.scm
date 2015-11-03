(declare (usual-integrations))

(define *ns-name* 'scheme)

;; Helper function: avoid environment-lookup throwing an error
(define (find-in-environment env name default)
  (let ((ns-bindings (environment-bound-names env)))
    (if (memq name ns-bindings)
	(environment-lookup env name)
	default)))

;; Helper syntax
(define-syntax if-let
  (syntax-rules ()
    ((if-let (?name ?expr) ?conseq)
     (let ((?name ?expr))
       (if ?name
	   ?conseq)))
    ((if-let (?name ?expr) ?conseq ?alter)
     (let ((?name ?expr))
       (if ?name
	   ?conseq
	   ?alter)))))

;; this one is useless for 'scheme namespace
;; but let's keep it for consistency
(define *provided-names*
  (environment-bound-names system-global-environment))

(define (provided-names #!optional ns)
  (let ((env (if (default-object? ns)
		 (nearest-repl/environment)
		 (namespace->environment ns))))
    (environment-lookup env '*provided-names*)))

(define (add-namespace! ns)
  (let ((ns-name (find-in-environment ns '*ns-name* #f)))
    (if ns-name
	(hash-table/put! *namespaces* ns-name ns)
	(error 'add-namespace! "Object is not a namespace" ns))))

(define (remove-namespace! ns-name)
  (if (eq? ns-name 'scheme)
      (error 'remove-namespace!
	     "Cannot remove root namespace")
      (hash-table/remove! *namespaces* ns-name)))

(define (change-namespace! ns-name)
  (let ((ns (or (hash-table/get *namespaces*
				ns-name
				#f)
		(error 'change-namespace!
		       "Cannot find namespace"
		       ns-name))))
    (ge ns)))

(define (list-namespaces)
  (hash-table/key-list *namespaces*))


(define (namespace->environment ns-name)
  (or (hash-table/get *namespaces* ns-name #f)
      (error 'namespace->environment
	     "Cannot find namespace"
	     ns-nname)))

(define (make-namespace name)
  (let ((ns (extend-top-level-environment
	     (namespace->environment 'scheme))))
    (environment-define ns '*ns-name* name)
    (environment-define ns '*provided-names* '())
    ns))

(define (import-namespace! target-ns ns-name included-names)
  (let ((target-env (namespace->environment target-ns))
	(imported-env (namespace->environment ns-name)))
    (if imported-env
	(let ((provided-names (provided-names ns-name)))
	  (for-each
	   (lambda (name)
	     (cond ((pair? name)
		    (let ((original-name (car name))
			  (subst-name (cdr name)))
		      (if (memq original-name provided-names)
			  (link-variables target-env
					  subst-name
					  imported-env
					  original-name)
			  (error 'load-namespace!
				 "Cannot find name in ns"
				 original-name
				 ns-name))))
		   ((symbol? name)
		    (if (memq name provided-names)
			(link-variables target-env
					name
					imported-env
					name)
			(error 'import-namespace!
			       "Cannot find name in ns"
			       name ns-name)))
		   (else (error 'load-namespace!
				"Unknown name"
				name))))
	   included-names))
	(error 'import-namespace! "Not a namespace" ns-name))))

(define (get-current-ns-name)
  (environment-lookup (nearest-repl/environment) '*ns-name*))


(define (import-namespace-with-prefix! target-ns ns-name prefix)
  (define (prefix-symbol s)
    (string->symbol (string-append prefix (symbol->string s))))
  (let* ((names (provided-names ns-name))
	 (prefixed-names (map (lambda (name)
				(cons name (prefix-symbol name)))
			      names)))
    (import-namespace! target-ns ns-name prefixed-names)))

(define (provide-names! ns-name . names)
  (let ((env (namespace->environment ns-name)))
    (environment-assign!
     env
     '*provided-names*
     (append names (environment-lookup env '*provided-names*)))))



(define-syntax import-opt
  (syntax-rules (prefix only)
    ((import-opt ?target-ns  (?ns-name (prefix ?prefix)))
     (import-namespace-with-prefix! '?target-ns
				    '?ns-name
				    (symbol->string '?prefix)))
    ((import-opt ?target-ns (?ns-name (only ?name ...)))
     (import-namespace! '?target-ns '?ns-name '(?name ...)))))


(define-syntax ns-opt
  (syntax-rules (file provide import)
    ((ns-opt ?name (file ?file-name))
     (load ?file-name (namespace->environment '?name)))
    ((ns-opt ?name (provide ?proc ...))
     (provide-names! '?name '?proc ...))
    ((ns-opt ?name (import ?ns-opts))
     (import-opt ?name ?ns-opts))))


(define-syntax ns
  (syntax-rules ()
    ((ns ?name ?opt1 ...)
     (begin
       (add-namespace! (make-namespace (quote ?name)))
       (ns-opt ?name ?opt1)
       ...))))













