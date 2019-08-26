#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {

  if [ -f ./cqlistener ]; then
    rm ./cqlistener
  fi
  if [ -f ./clipqueue ]; then
    rm ./clipqueue
  fi
  echo "building node process"
    (cd CQKeyListener && npm run build)
  echo "building haskell process"
    (cd CQui && stack build --copy-bins)
  mkdir -p "~/local/bin" && cp ./cqlistener ~/.local/bin
}

main
