;;; x-modules.el --- A listing of modules to load on startup


;;; Commentary:

;; This file is just a list of EmacsX modules to load on startup.
;; For convenience the modules are grouped in several categories.
;; The x-modules.el in the samples folder should be copied
;; to your personal folder and edited there.

;; Note that some modules can't be used together - e.g. you shouldn't
;; enable both x-ido and x-ivy, as they serve the same
;; purpose.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;; Uncomment the modules you'd like to use and restart EmacsX afterwards

;;; General productivity tools

;; (require 'x-ido) ;; Supercharges Emacs completion for C-x C-f and more
;; (require 'x-ivy) ;; A mighty modern alternative to ido
(require 'x-vertico) ;; A powerful, yet simple, alternative to ivy
;; (require 'x-helm) ;; Interface for narrowing and search
;; (require 'x-helm-everywhere) ;; Enable Helm everywhere
(require 'x-company)
;; (require 'x-key-chord) ;; Binds useful features to key combinations

;;; Vim emulation
;; Enable this module if you're fond of vim's keybindings.
;; (require 'x-evil)

;;; Org-mode (a legendary productivity tool that deserves its own category)
;; Org-mode helps you keep TODO lists, notes and more.
(require 'x-org)

;;; Programming languages support
;; Modules for a few very common programming languages
;; are enabled by default.

(require 'x-c)
;; (require 'x-clojure)
;; (require 'x-coffee)
;; (require 'x-common-lisp)
(require 'x-css)
;; (require 'x-dart)
(require 'x-emacs-lisp)
;; (require 'x-erlang)
;; (require 'x-elixir)
;; (require 'x-fsharp)
;; (require 'x-go)
;; (require 'x-haskell)
(require 'x-js)
;; (require 'x-latex)
(require 'x-lisp) ;; Common setup for Lisp-like languages
;; (require 'x-literate-programming) ;; Setup for Literate Programming
(require 'x-lsp) ;; Base setup for the Language Server Protocol
;; (require 'x-lua)
;; (require 'x-ocaml)
(require 'x-perl)
;; (require 'x-python)
;; (require 'x-racket)
;; (require 'x-ruby)
;; (require 'x-rust)
;; (require 'x-scala)
;; (require 'x-scheme)
(require 'x-shell)
;; (require 'x-scss)
;; (require 'x-ts)
(require 'x-web) ;; Emacs mode for web templates
(require 'x-xml)
(require 'x-yaml)

;;; Misc
(require 'x-erc) ;; A popular Emacs IRC client (useful if you're still into Freenode)

(provide 'x-modules)
;;; x-modules.el ends here
