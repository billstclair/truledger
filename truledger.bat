REM Usage is: "truledger [slimeport]"
erase src\*.*fsl
ccl -e "(load \"truledger-loader.lisp\")" -e "(when (find-package :truledger) (in-package :truledger))" -e "(truledger-loader:load-swank %1)"
