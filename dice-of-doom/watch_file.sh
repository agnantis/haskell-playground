#!/bin/bash

filename=$1
echo "Watch for changes in: '$1'"
while true; do
  change=$(inotifywait -q -e close_write .)
  change=${change#./ * }
  if [ "$change" = "$filename" ]; then 
    ghc -hidir /tmp $filename
    res=$?
    [[ $res == 0 ]] && hlint $filename
  fi
done
