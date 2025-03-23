;;; x-user.el --- A listing of user modules to load on startup

;;; Commentary:
;; For convenience the modules are grouped in several categories.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defvar x:user-dir (expand-file-name "user" x:init-dir)
  "The home for inital functionality.")

;; Add directories to load path
(add-to-list 'load-path x:user-dir)

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
;; (require 'x-erc) ;; A popular Emacs IRC client (useful if you're still into Freenode)

(provide 'x-user)
;;; x-user.el ends here
