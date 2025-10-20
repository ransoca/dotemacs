#!/usr/bin/env bash

EMACS="$(brew --prefix)/opt/emacs-plus/Emacs.app/Contents/MacOS/Emacs"

${EMACS} ${PWD} > /dev/null 2>&1 &
