REM Usage is: "truledger [slimeport]"

ccl -e "(load \"start\")" -e "(when (find-package :truledger) (in-package :truledger))" -e "(cl-user::start-swank %1)"
