REM Load Truledger and save application
erase src\*.*fsl
ccl -e "(load \"truledger-loader.lisp\")" -e "(when (find-package :truledger) (in-package :truledger))" -e "(truledger:save-truledger-application)"
