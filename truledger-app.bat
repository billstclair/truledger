REM Load Truledger and save application
ccl -e "(load \"start\")" -e "(when (find-package :truledger) (in-package :truledger))" -e "(truledger:save-truledger-application)"
