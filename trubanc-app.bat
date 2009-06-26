REM Load Trubanc and save application
erase src\*.*fsl
ccl -e "(load \"trubanc-loader.lisp\")" -e "(when (find-package :trubanc) (in-package :trubanc))" -e "(trubanc:save-trubanc-application)"
