;;; A package definition for the scsh-install-lib

(define-package "prometheus" (2)
  ((install-lib-version (1 0)))

  (install-file '("scheme/packages.scm" . "load.scm") 'base)
  (install-files '("scheme/hermes.scm"
                   "scheme/prometheus.scm")
                 'scheme)
  (install-files '("README"
                   "NEWS")
                 'doc)
  )
