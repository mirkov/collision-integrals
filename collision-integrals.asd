(asdf:defsystem :collision-integrals
  :components
  ;; file naming convention: omega-r- files refer to `r'educed Omega
  ;; integrals
  ((:module "package"
	    :pathname #p"./"
	    :components ((:file "collision-integrals-package-def")))
   (:module "hard-sphere-omega"
	    ;; omega calculations for the hard-sphere potential
	    :pathname #p"./"
	    :depends-on ("package")
	    :components ((:file "omega-hs")))
   (:module "lennard-jones-omega*"
	    ;; omega* calculations for the lennard-jones potential
	    ;; some of the components will need to be moved out if I
	    ;; add additional potentials
	    :pathname #p"./"
	    :depends-on ("package")
	    :components ((:file "lj-collision-integral-fits")
			 (:file "lj-collision-integral-table-interpolations")
			 (:file "omega-r-setup")))
   (:module "general-potential-omega"
	    ;; omega calculations for the lennard-jones potential
	    :pathname #p"./"
	    :depends-on ("hard-sphere-omega"
			 "lennard-jones-omega*")
	    :components ((:file "collision-parameters")
			 (:file "omega-gf+defaults")))
   (:module "omega*-combinations"
	    :pathname #p"./"
	    :depends-on ("package")
	    :components ((:file "omega-r-combinations")))
   (:module "default-methods"
	    ;; machinery for examinining and modifying default methods
	    ;; for omega* for the lennard-jones potential
	    :pathname #p"./"
	    :depends-on ("lennard-jones-omega*")
	    :components ((:file "lj-default-methods"))))
  :depends-on (:lisp-unit
	       :alexandria
	       :my-utils
	       :mv-grid-utils
	       :mv-grid+gsll-utilities
	       :molecular-potentials
	       :physics-constants))

(asdf:defsystem :collision-integrals-user
  :components ((:file "collision-integrals-user-package-def")
	       (:file "collision-integrals-user"
		      :depends-on ("collision-integrals-user-package-def")))
  :depends-on (:collision-integrals
	       :molecular-potentials
	       :periodic-table
	       :physics-constants
	       :lisp-unit))