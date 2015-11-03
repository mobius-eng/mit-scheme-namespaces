Namespace for MIT-Scheme
========================

MIT-Scheme has an internal package system, however it is not
documented which makes it tricky to use.

At the same time, MIT-Scheme supports first class environments
(in fact packaging system is implemented using environments). Namespaces
are implemented using environments and, as a result, completely
embed within existing Scheme.

# TL;DR Usage

Quick disclaimer: the use of namespaces is slightly clanky as it requires
external *interface* or *header* file to declare the namespace. The reason:
cannot control the order of source file evaluation (as with `EVAL-WHEN` in
Common Lisp).

Example files provide full source example of use. First, the interface
`example-ns`:

```Scheme
(ns example
    (file "example")
    (provide sub1 factorial))

```
Here we define a new namespace `example` the source-code for which
resides inside "example.scm" file (or compiled "example.com");
this namespace provides the names `sub1` and `factorial`.

Implementation file is usual Scheme-file:

```Scheme
(declare (usual-integrations))

(define (sub1 n) (-1+ n))

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

```

We use this namespace inside another namespace `example-use`
defined in interface file `example-use-ns.scm` as following:

```Scheme
(ns example-use
    (import (example (prefix ex:)))
    (file "example-use"))
```
Here we import all provided names from `example` giving them
prefix `ex:`, i.e. they will be accessible as `ex:sub1` and
`ex:factorial`. The use is trivial (`example-use.scm`):

```Scheme
(display (ex:factorial 5))
(newline)
```
# Compilation

Call `cf` on all files in the repository.

TODO: include compile script.

# Preload namespaces into Scheme

Put the following into your `~/.scheme.init`:
```Scheme
(define start-location (pwd))
(begin
  (cd "path-to/namespaces")
  (load "ns-load")
  (cd start-location))
```

Alternatively, you can save the image of Scheme after loading
"ns-load".

# Effect and initial state
Originally two namespaces are defined: `scheme` and `scheme-user`.
REPL starts in `scheme-user`, all user-defined namespaces import
all bindings of `scheme`. It is planned for the future to make it
more selective if someone wants to define their own primitives.

`scheme` namespace is basically the extension of
`system-global-environment` with added procedures and syntax to
support namespaces. Thus any previous Scheme code should still
with namespaces.

# Syntax

```Scheme
(ns <new-ns-name> <clauses>)
```
defines new namespace named `<new-ns-name>`. Supported clauses:

- `(file "filename")` relative path to a source file with
implementation
- `(import <import-options>)` imports another namespace into
current. See below for import options.
- `(provide <name> ...)` list of provided (public) names.

Import-options:
- `(import (<ns-name> (prefix <prefix>)))` imports all public
names from `<ns-name>` by adding `<prefix>` to their names.
- `(import (<ns-name> (only <name> ...)))` imports (unaltered)
only specified public names from `<ns-name>`.

# Underlying machinery

A namespace is basically a Scheme environment with the following
properties:
- It extends from `scheme` namespace
- It contains the following variables:
  + `*ns-name*` name of the current namespace
  + `*provuided-names*` list of namespace public names

Root namespace `scheme` declares the following variables and
procedures:
- `*namespaces*`: a hash-table consisting all loaded namespaces
- `*ns-name*` with value `scheme`
- `*provided-names*`: all names from the global environment
- Procedure `(privded-names [ns-name])` shows the list of public
names from the namespace (or current namespace if parameter is
omitted). `ns-name` must be a symbol.
- `(make-namespace ns-name)`: creates new namespace with the
name `ns-name`.
- `(add-namespace! ns)` adds new namespace to the list of loaded
namespaces. `ns` must be an environment with namespace properties.
Note, unless added, newly created namespaces are not available.
- `(remove-namespace! ns-name)` removes the namespace named
`ns-name` from the list of available namespaces.
- `(change-namespace! ns-name)` switches current context to
a namespace names `ns-name`.
- `(list-namespaces)` creates newly allocated list of all available
namespaces
- `(namespace->environment ns-name)` gets the environment object
of the namespace named `ns-name`
- `(import-namespace! target-ns-name ns-name included-names)`
imports names from `ns-name` into `target-ns-name`. `included-names`
is the list of names to be imported. Each entry in the list is either
a symbol which must be a member of public names of `ns-name` or a
pair `(new-name . original-name)` in which case the value of
`original-name` in `ns-name` is imported under `new-name`.
- `(get-current-ns-name)` returns the name of the current
namespace
- `(import-namespace-with-prefix! target-ns-name ns-name prefix)`
interface to `import-namespace!` in case of importing all the
names from `ns-name` by prefixing them with `prefix` (string).
- `(provide-names! ns-name . names)` adds `names` (list of symbols)
to the list of public names of `ns-name`.

# State

Currently this package is at alpha state: not quite tested and
API might change.

# Licence

LGPL 3
