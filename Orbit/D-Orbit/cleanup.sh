#!/bin/bash

##
## orbit-int script for setting/cleaning up distributed Erlang nodes.
##
## Author: Patrick Maier <P.Maier@hw.ac.uk>
##


# Enable job control
set -m


# Some variables fixed at beginning
DIR=${PWD}   ## current working directory
NODES=$*     ## list of Erlang nodes (short format 'name@host')


# Start Erlang nodes (arg: list of nodes in short format)
startErls() {
  local NODE

  echo -n "Starting Erlang nodes ... "
  for NODE in $*; do
    ssh ${NODE#*@} "cd ${DIR}; erl -detached -sname ${NODE}" 2>/dev/null
  done
  echo started
}


# Remote cleanup function (arg: list of nodes in format 'name@host')
function killErls() {
  local NODE

  echo -n "Stopping Erlang nodes ... "
  for NODE in $*; do
    ssh ${NODE#*@} "pkill '"'beam*|epmd'"'" 2>/dev/null
  done
  echo done
}


# Dispatch on script name
case "${0##*/}" in
  setup.sh)   # Set cleanup trap, start Erlang ${NODES}, and suspend
              trap "killErls ${NODES}" EXIT
              startErls ${NODES}
              suspend;;

  cleanup.sh) # Cleanup Erlang ${NODES}
              killErls ${NODES};;

  *)          # Exit with error
              exit 1;;
esac


# Exit regularly (trap will clean up)
exit 0
