Namespace for MIT-Scheme
========================

MIT-Scheme has an internal package system, however it is not
documented which makes it tricky to use.

At the same time, MIT-Scheme supports first class environments
(in fact packaging system is implemented using environments). Namespaces
are implemented using environments and, as a result, completely
embed within existing Scheme.

Current implementation uses packages convention: namespaces create
hierachy of nested modules. At the moment, however, it is only
organizational tool: namespaces do not inherit values, procedures or
macros automatically from the enclosing namespace.

# TL;DR Usage

Quick disclaimer: the use of namespaces is slightly clanky as it
requires external *interface* or *header* file to declare the
namespace. The reason: cannot control the order of source file
evaluation in Scheme (as with `EVAL-WHEN` in Common Lisp).

Example files provide full source example of use. First, the interface
`example-ns`:

```Scheme
(ns (scheme example)
    (file "example")
    (provide sub1 factorial)
    (provide-syntax when))
```

Here we define a new namespace `(scheme example)` which resides
inside namespace `(scheme)` (root to all other namespaces). The
source-code for `(scheme example)` is loaded from "example.scm" file
(the file is compiled in the process to "example.com"). This namespace
provides the names `sub1` and `factorial` and syntax (macro)
definition `when`.

Implementation file is usual Scheme-file:

```Scheme
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

```

We use this namespace inside another namespace `(scheme example
example-use)` defined in interface file `example-use-ns.scm` as
following:

```Scheme
(ns (scheme example example-use)
    (import ((scheme example) (prefix ex:)))
    (import-syntax ((scheme example) (only when)))
    (file "example-use"))
```

Here we import all provided names from `(scheme example)` giving them
prefix `ex:`, i.e. they will be accessible as `ex:sub1` and
`ex:factorial`. The use is trivial (`example-use.scm`):

```Scheme
(newline)
(display (ex:factorial 5))
(newline)

(when #t
      (display "Yay!")
      (newline))


```

# Compilation

Use the following script to compile and load namespaces either use
`(load "ns-load")` or add the following lines into `.scheme.init`:

```Scheme
(define current-location (pwd))

(define project-directory "/path/to/Scheme/projects/")

(cd (string-append project-directory "mit-scheme-namespaces/"))
(load "ns-load")
;; (cd (string-append project-directory "mit-scheme-utils/"))
;; (load "load")
(cd current-location)
```
You can also save the band file with preloaded namespaces. To do so
you must start mit-scheme (or scmutils) *without* `--edit` option!
After namespaces are loaded, execute:

```Scheme
(disk-save "name-of-the-band-file" "Scheme with namespaces")
```

and start Scheme with the saved band (see MIT-Scheme user guide for
options).

# Limitations

Currently the namespaces are implemented for `scmutils` (classical
mechanics framework for scheme). One change would be required to make
it work for mit-scheme: in file "root-namespace.scm" the environment
from which `root-environment` is extended needs to be replaced from
`generic-environment` to `system-global-environment`. *Warning*: it
was not tested on Scheme without `scmutils`, proceed with caution.

# Effect and initial state

Originally two namespaces are defined: `(scheme)` and `(scheme user)`.
REPL starts in `(scheme user)`, all user-defined namespaces import all
bindings of `(scheme)`. It is planned for the future to make it more
selective if someone wants to define their own primitives.

`(scheme)` namespace is basically the extension of
`system-global-environment` (`generic-environment` for `scmutils`)
with added procedures and syntax to support namespaces. Thus any
previous Scheme code should still work with namespaces.

# Syntax

```Scheme
(ns <new-ns-name> <clauses>)
```
defines new namespace named `<new-ns-name>`. Name should be a list of
the form `(scheme n-1 n-2 ... n-i)`:

- All modules `n-1`, ..., `n-(i-1)` must exist already.
- Module `n-i` is being defined and nested inside `(scheme n-1
  ... n-(i-1))`.

Supported clauses:

- `(file "filename")` relative path to a source file with
implementation
- `(import <import-options>)` imports another namespace into
current. This clasue only imports variable names (not macros). See
below for import options.
- `(provide <name> ...)` lists provided (public) names (variables).
- `(import-syntax <options>)` imports syntax (macros) from another
namespaces. See below for options.
- `(provde-syntax <macro> ...)` lists provided (public) macros.

Import-options:
- `(import (<ns-name> (prefix <prefix>)))` imports all public
names from `<ns-name>` by adding `<prefix>` to their names.
- `(import (<ns-name> (only <name> ...)))` imports (unaltered)
only specified public names from `<ns-name>`.

The same options exist for `import-syntax`.


# Underlying machinery

A namespace is basically a Scheme environment with the following
properties:
- It extends from `(scheme)` namespace
- It contains the following variables:
  + `*ns-name*` name of the current namespace
  + `*provided-names*` list of namespace public names
  + `*provided-macros*` list of public macros

Root namespace `(scheme)` declares the following variables and
procedures:
- `*namespaces*`: a hash-table consisting all loaded namespaces. This
  hastable has a hierachichal structure. At the root it contains only
  one key `scheme` (root environment). Each entry is a pair with `car`
  being a namespace and `cdr` being a hash-table of nestes namespaces.
- `*ns-name*` with value `(scheme)`
- `*provided-names*`: all names from the global environment
- Procedure `(ns/privded-names [ns-name])` shows the list of public
names from the namespace (or current namespace if parameter is
omitted). `ns-name` must be a symbol.
- Procedure `(ns/provided-macros [ns-name])` returns the list of
  public macros of `[ns-name]` or current namespace if not specified.
- `(make-namespace parent ns-name)`: creates new namespace with the
name `(append parent (list ns-name))`.
- `(change-namespace! ns-name)` switches current context to
a namespace names `ns-name`.
- `(list-namespaces)` creates newly allocated list of all available
namespaces
- `(namespace->environment ns-name)` gets the environment object
of the namespace named `ns-name`
- `(ns/import-names! ns-name included-names [target-ns])`
imports names from `ns-name` into `target-ns-name`. `included-names`
is the list of names to be imported. Each entry in the list is either
a symbol which must be a member of public names of `ns-name` or a
pair `(new-name . original-name)` in which case the value of
`original-name` in `ns-name` is imported under `new-name`.
- `(ns/import-macros! ns-name included-macros [target-ns])` the same
  as above but for macros.
- `(get-current-ns-name)` returns the name of the current
namespace
- `(ns/import-names-with-prefix! ns-name names prefix [target-ns])`
and `(ns/import-macros-with-prefix! ns-name macros prefix
[target-ns])` interfaces to `ns/import-names!` and `ns/import-macros!`
in case of importing all the names from `ns-name` by prefixing them
with `prefix` (symbol).
- `(ns/add-to-provided-names! ns-name . names)` adds `names` (list of symbols)
to the list of public names of `ns-name`.
- `(ns/add-to-provided-macros! ns-name . macros)` the same as above
  for macros

# State

As there is quite a lot to improve, functionality might change quite
rapidly. Use with caution.

# Licence

LGPL 3
