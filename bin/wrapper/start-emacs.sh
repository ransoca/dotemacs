#!/usr/bin/env bash

EMACS_SH="/Applications/Emacs.app/Contents/MacOS/Emacs.sh"

${EMACS_SH} ${PWD} > /dev/null 2>&1 &
