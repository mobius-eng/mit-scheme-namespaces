(declare (usual-integrations))

;; this one is useless for 'scheme namespace
;; but let's keep it for consistency
(define *name* '(scheme))

(define *provided-names*
  (environment-bound-names generic-environment))

(define *provided-macros* (environment-macro-names generic-environment))

(define (find-namespace-structure ns)
  (let loop ((n ns)
	     (table *namespaces*))
    (cond ((not (pair? n)) (values #f #f (list 'bad-namespace ns)))
	  ((null? (cdr n))
	   (let ((entry (hash-table/get table (car n) #f)))
	     (if entry
		 (values entry #t #f)
		 (values #f #f (list 'not-found ns n)))))
	  (else (let ((entry (hash-table/get table (car n) #f)))
		  (if entry
		      (loop (cdr n) (cdr entry))
		      (values #f #f (list 'not-found ns n))))))))

(define (get-namespace-entry ns)
  (receive (entry found? error-msg) (find-namespace-structure ns)
    (if found?
	entry
	(error 'get-namespace-entry "Error" error-msg))))

(define (namespace->environment ns)
  (receive (entry found? error-msg) (find-namespace-structure ns)
    (if found?
	(car entry)
	(case (car error-msg)
	  ((bad-namespace) (error 'namespace->envrionment
				  "Bad namespace"
				  (cdr error-msg)))
	  ((not-found) (error 'namespace->environment
			      "Cannot find namespace"
			      (cdr error-msg)))
	  (else (error 'namespace->environment
		       "Error"
		       error-msg))))))

(define (find-namespace-environment ns)
  (receive (entry found? error-msg) (find-namespace-structure ns)
    (and found? (car entry))))

(define ns->env namespace->environment)


(define (namespace? ns)
  (and (pair? ns)
       (find-namespace-environment ns)))

(define ns? namespace?)

(define (ns/provided-names #!optional ns)
  (let ((env (if (default-object? ns)
		 (nearest-repl/environment)
		 (namespace->environment ns))))
    (environment-lookup env '*provided-names*)))

(define (ns/provided-macros #!optional ns)
  (let ((env (if (default-object? ns)
		 (nearest-repl/environment)
		 (namespace->environment ns))))
    (environment-lookup env '*provided-macros*)))

(define (change-namespace! ns-name)
  (ge (ns->env ns-name)))

(define (namespace-define ns . symbol-values)
  (let ((env (namespace->environment ns)))
    (let loop ((symbol-values symbol-values))
      (cond ((null? symbol-values) 'done)
	    ((null? (cdr symbol-values))
	     (error 'namespace-define
		    "No value provided for symbol"
		    (car symbol-values)))
	    (else
	     (environment-define env
				 (car symbol-values)
				 (cadr symbol-values))
	     (loop (cddr symbol-values)))))))

(define (make-namespace parent ext-name)
  (let ((parent-entry (if (null? parent)
			  (cons '() *namespaces*)
			  (get-namespace-entry parent))))
    (let ((table (cdr parent-entry))
	  (name (append parent (list ext-name)))
	  (env (extend-top-level-environment
		(namespace->environment '(scheme)))))
      (hash-table/put! table
		       ext-name
		       (cons env
			     (make-strong-eq-hash-table)))
      (namespace-define name
			'*ns-name* name
			'*provided-names* '()
			'*provided-macros* '())
      (append parent (list ext-name)))))

(define (ns/import-names!  from-ns included-names #!optional target-ns)
  (let ((target-env (if (default-object? target-ns)
			(nearest-repl/environment)
			(namespace->environment target-ns)))
	(imported-env (namespace->environment from-ns))
	(provided-names (ns/provided-names from-ns)))
    (for-each
     (lambda (name)
       (let ((original-name (if (pair? name) (car name) name))
	     (subst-name (if (pair? name) (cdr name) name)))
	 (if (memq original-name provided-names)
	     (link-variables target-env
			     subst-name
			     imported-env
			     original-name)
	     (error 'ns/import-names!
		    "Cannot find name in ns"
		    name
		    from-ns))))
     included-names)))

(define (ns/import-macros! source-ns macros #!optional target-ns)
  (let ((target-env (if (default-object? target-ns)
			(nearest-repl/environment)
			(namespace->environment target-ns)))
	(source-env (namespace->environment source-ns))
	(source-macros (ns/provided-macros source-ns)))
    (for-each
     (lambda (macro)
       (let ((original-macro (if (pair? macro) (car macro) macro))
	     (new-macro (if (pair? macro) (cdr macro) macro)))
	 (if (memq original-macro source-macros)
	     (environment-define-macro
	      target-env
	      new-macro
	      (environment-lookup-macro source-env original-macro))
	     (error 'ns/import-macros!
		    "Cannot find macro in namespace"
		    original-macro
		    source-ns))))
     macros)))

(define (get-current-ns-name)
  (environment-lookup-or (nearest-repl/environment) '*ns-name* #f))

(define (ns/import-names-with-prefix! ns-name names prefix #!optional target-ns)
  (define prefix-string (symbol->string prefix))
  (define (prefix-symbol s)
    (string->symbol (string-append prefix-string (symbol->string s))))
  (let ((prefixed-names (map (lambda (name)
			       (cons name (prefix-symbol name)))
			     names)))
    (ns/import-names! ns-name prefixed-names target-ns)))

(define (ns/add-to-provided-names! ns-name . names)
  (let ((env (namespace->environment ns-name)))
    (environment-assign!
     env
     '*provided-names*
     (append names (environment-lookup env '*provided-names*)))))

(define (ns/import-macros-with-prefix! source-ns macros prefix #!optional target-ns)
  (define prefix-string (symbol->string prefix))
  (define (prefix-symbol s)
    (string->symbol (string-append prefix-string (symbol->string s))))
  (let ((prefixed-macros (map (lambda (macro)
				(cons macro (prefix-symbol macro)))
			      macros)))
    (ns/import-macros! source-ns prefixed-macros target-ns)))

(define (ns/add-to-provided-macros! ns-name . macros)
  (let ((env (namespace->environment ns-name)))
    (environment-assign!
     env
     '*provided-macros*
     (append macros (environment-lookup env '*provided-macros*)))))

(define-syntax import-opt
  (syntax-rules (prefix only)
    ((import-opt ?target-ns  (?ns-name (prefix ?prefix)))
     (ns/import-names-with-prefix! '?ns-name
				   (ns/provided-names '?ns-name)
				   '?prefix
				   '?target-ns))
    ((import-opt ?target-ns (?ns-name (only ?name ...)))
     (ns/import-names! '?ns-name '(?name ...) '?target-ns))))

(define-syntax import-macros-opt
  (syntax-rules (prefix only)
    ((import-macros-opt ?target-ns (?source-ns (prefix ?prefix)))
     (ns/import-macros-with-prefix! '?source-ns
				    (ns/provided-macros '?source-ns)
				    '?prefix
				    '?target-ns))
    ((import-macros-opt ?target-ns (?source-ns (only ?macro ...)))
     (ns/import-macros! '?source-ns '(?macro ...) '?target-ns))))

(define-syntax ns-opt
  (syntax-rules (file provide import provide-syntax import-syntax)
    ((ns-opt ?name (file ?file-name))
     (begin (compile-file ?file-name '() (namespace->environment '?name))
	    (load ?file-name (namespace->environment '?name))))
    ((ns-opt ?name (provide ?proc ...))
     (ns/add-to-provided-names! '?name '?proc ...))
    ((ns-opt ?name (import ?ns-opts))
     (import-opt ?name ?ns-opts))
    ((ns-opt ?name (provide-syntax ?macro ...))
     (ns/add-to-provided-macros! '?name '?macro ...))
    ((ns-opt ?name (import-syntax ?macro-opts))
     (import-macros-opt ?name ?macro-opts))))

(define-syntax ns
  (syntax-rules ()
    ((ns ?name ?opt1 ...)
     (let ((nm '?name))
       (make-namespace (butlast nm) (last nm))
       (ns-opt ?name ?opt1)
       ...))))

(define (list-namespaces)
  (define (list-ns-loop table prefix)
    (let* ((modules (hash-table/key-list table))
	   (cur-level-ns (map (lambda (m) (append prefix (list m)))
			      modules)))
      (if (null? modules)
	  '()
	  (append cur-level-ns
		  (mapcan (lambda (m ns-name)
			    (list-ns-loop (cdr (hash-table/get table m #f))
					  ns-name))
			  modules
			  cur-level-ns)))))
  
  (list-ns-loop *namespaces* '()))











