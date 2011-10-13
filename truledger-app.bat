REM Load Truledger and save application
copy VERSION appwin\version.txt
ccl -e "(load \"start\")" -e "(when (find-package :truledger) (in-package :truledger))" -e "(truledger:save-truledger-application)"
