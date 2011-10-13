IF EXIST "c:\Program Files (x86)\Inno Setup 5\iscc.exe". (
   "c:\Program Files (x86)\Inno Setup 5\iscc"  %1.
) ELSE (
  "c:\Program Files\Inno Setup 5\iscc"  %1.
)

