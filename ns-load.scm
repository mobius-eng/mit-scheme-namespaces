(declare (usual-integrations))

(compile-file "root-namespace" '())
(load "root-namespace")
(compile-file "namespaces" '() root-environment)
(load "namespaces" root-environment)
(compile-file "scheme-user" '() root-environment)
(load "scheme-user" root-environment)
'(scheme user)

