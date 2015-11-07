(declare (usual-integrations))

(define *ns-name* 'scheme)

;; Helper function: avoid environment-lookup throwing an error
(define (find-in-environment env name default)
  (let ((ns-bindings (environment-bound-names env)))
    (if (memq name ns-bindings)
	(environment-lookup env name)
	default)))

;; this one is useless for 'scheme namespace
;; but let's keep it for consistency
(define *provided-names*
  (environment-bound-names generic-environment))

(define *provided-macros* (environment-macro-names generic-environment))

(define (provided-names #!optional ns)
  (let ((env (if (default-object? ns)
		 (nearest-repl/environment)
		 (namespace->environment ns))))
    (environment-lookup env '*provided-names*)))

(define (provided-macros #!optional ns)
  (let ((env (if (default-object? ns)
		 (nearest-repl/environment)
		 (namespace->environment ns))))
    (environment-lookup env '*provided-macros*)))

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
    (environment-define ns '*provided-macros* '())
    ns))

(define (import-names! target-ns ns-name included-names)
  (let* ((target-env (namespace->environment target-ns))
	 (imported-env (namespace->environment ns-name))
	 (provided-names (provided-names ns-name)))
    (for-each
     (lambda (name)
       (let ((original-name (if (pair? name) (car name) name))
	     (subst-name (if (pair? name) (cdr name) name)))
	 (if (memq original-name provided-names)
	     (link-variables target-env
			     subst-name
			     imported-env
			     original-name)
	     (error 'load-namespace!
		    "Cannot find name in ns"
		    name
		    ns-name))))
     included-names)))

(define (import-macros! target-ns source-ns macros)
  (let* ((target-env (namespace->environment target-ns))
	 (source-env (namespace->environment source-ns))
	 (source-macros (provided-macros source-ns)))
    (for-each
     (lambda (macro)
       (let ((original-macro (if (pair? macro) (car macro) macro))
	     (new-macro (if (pair? macro) (cdr macro) macro)))
	 (if (memq original-macro source-macros)
	     (environment-define-macro
	      target-env
	      new-macro
	      (environment-lookup-macro source-env original-macro))
	     (error 'import-macros!
		    "Cannot find macro in namespace"
		    original-macro
		    source-ns))))
     macros)))


(define (get-current-ns-name)
  (environment-lookup (nearest-repl/environment) '*ns-name*))


(define (import-names-with-prefix! target-ns ns-name prefix)
  (define (prefix-symbol s)
    (string->symbol (string-append prefix (symbol->string s))))
  (let* ((names (provided-names ns-name))
	 (prefixed-names (map (lambda (name)
				(cons name (prefix-symbol name)))
			      names)))
    (import-names! target-ns ns-name prefixed-names)))

(define (provide-names! ns-name . names)
  (let ((env (namespace->environment ns-name)))
    (environment-assign!
     env
     '*provided-names*
     (append names (environment-lookup env '*provided-names*)))))


(define (import-macros-with-prefix! target-ns source-ns prefix)
  (define (prefix-symbol s)
    (string->symbol (string-append prefix (symbol->string s))))
  (let* ((macros (provided-macros source-ns))
	 (prefixed-macros (map (lambda (macro)
				 (cons macro (prefix-symbol macro)))
			       macros)))
    (import-macros! target-ns source-ns prefixed-macros)))

(define (provide-macros! ns-name . macros)
  (let ((env (namespace->environment ns-name)))
    (environment-assign!
     env
     '*provided-macros*
     (append macros (environment-lookup env '*provided-macros*)))))

(define-syntax import-opt
  (syntax-rules (prefix only)
    ((import-opt ?target-ns  (?ns-name (prefix ?prefix)))
     (import-names-with-prefix! '?target-ns
				'?ns-name
				(symbol->string '?prefix)))
    ((import-opt ?target-ns (?ns-name (only ?name ...)))
     (import-names! '?target-ns '?ns-name '(?name ...)))))

(define-syntax import-macros-opt
  (syntax-rules (prefix only)
    ((import-macros-opt ?target-ns (?source-ns (prefix ?prefix)))
     (import-macros-with-prefix! '?target-ns
				 '?source-ns
				 (symbol->string '?prefix)))
    ((import-macros-opt ?target-ns (?source-ns (only ?macro ...)))
     (import-macros! '?target-ns '?source-ns '(?macro ...)))))

(define-syntax ns-opt
  (syntax-rules (file provide import provide-syntax import-syntax)
    ((ns-opt ?name (file ?file-name))
     (load ?file-name (namespace->environment '?name)))
    ((ns-opt ?name (provide ?proc ...))
     (provide-names! '?name '?proc ...))
    ((ns-opt ?name (import ?ns-opts))
     (import-opt ?name ?ns-opts))
    ((ns-opt ?name (provide-syntax ?macro ...))
     (provide-macros! '?name '?macro ...))
    ((ns-opt ?name (import-syntax ?macro-opts))
     (import-macros-opt ?name ?macro-opts))))

(define-syntax ns
  (syntax-rules ()
    ((ns ?name ?opt1 ...)
     (begin
       (add-namespace! (make-namespace (quote ?name)))
       (ns-opt ?name ?opt1)
       ...))))













