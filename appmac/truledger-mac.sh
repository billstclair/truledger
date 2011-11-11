if [[ -f truledger-dx86cl64 && $HOSTTYPE == 'x86_64' ]]; then
  ./truledger-dx86cl64;
elif [[ -f truledger-dx86cl32 ]]; then
  ./truledger-dx86cl32;
else
  echo "Can't find executable.";
  while true; do sleep 1; done;
fi
