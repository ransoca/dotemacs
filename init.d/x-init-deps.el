;;; x-init-deps.el --- Foundational Dependencies.

;;; Commentary:
;; Takes care of the automatic installation of all the packages.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'package)

;;;; Package setup and additional utility functions

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" x:dir))

(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)

(setq package-pinned-packages
      '((ace-window . "melpa-stable")
        (alchemist . "melpa-stable")
        (anaconda-mode . "melpa-stable")
        (anzu . "melpa-stable")
        (async . "melpa-stable")
        (avy . "melpa-stable")
        (browse-kill-ring . "melpa-stable")
        (caml . "melpa-stable")
        (cask-mode . "melpa-stable")
        (cdlatex . "melpa-stable")
        (cider . "melpa-stable")
        (clojure-mode . "melpa-stable")
        (cmake-mode . "melpa-stable")
        (coffee-mode . "melpa-stable")
        (company . "melpa-stable")
        (company-anaconda . "melpa-stable")
        (company-auctex . "melpa-stable")
        (company-go . "melpa-stable")
        (crux . "melpa-stable")
        (cython-mode . "melpa-stable")
        (d-mode . "melpa-stable")
        (dart-mode . "melpa-stable")
        (diff-hl . "melpa-stable")
        (diminish . "melpa-stable")
        (discover-my-major . "melpa-stable")
        (dockerfile-mode . "melpa-stable")
        (easy-kill . "melpa-stable")
        (elisp-slime-nav . "melpa-stable")
        (elixir-mode . "melpa-stable")
        (elm-mode . "melpa-stable")
        (epl . "melpa-stable")
        (erlang . "melpa-stable")
        (evil . "melpa-stable")
        (evil-numbers . "melpa-stable")
        (evil-surround . "melpa-stable")
        (evil-visualstar . "melpa-stable")
        (exec-path-from-shell . "melpa-stable")
        (expand-region . "melpa-stable")
        (f . "melpa-stable")
        (feature-mode . "melpa-stable")
        (flx . "melpa-stable")
        (flx-ido . "melpa-stable")
        (flycheck . "melpa-stable")
        (flycheck-ocaml . "melpa-stable")
        (geiser . "melpa-stable")
        (gh . "melpa-stable")
        (gist . "melpa-stable")
        (git-commit . "melpa-stable")
        (git-timemachine . "melpa-stable")
        (git-modes . "melpa-stable")
        (go-eldoc . "melpa-stable")
        (go-guru . "melpa-stable")
        (go-mode . "melpa-stable")
        (go-projectile . "melpa-stable")
        (go-rename . "melpa-stable")
        (gotest . "melpa-stable")
        (goto-chg . "melpa-stable")
        (grizzl . "melpa-stable")
        (groovy-mode . "melpa-stable")
        (guru-mode . "melpa-stable")
        (haml-mode . "melpa-stable")
        (haskell-mode . "melpa-stable")
        (helm . "melpa-stable")
        (helm-ag . "melpa-stable")
        (helm-core . "melpa-stable")
        (helm-descbinds . "melpa-stable")
        (helm-projectile . "melpa-stable")
        (imenu-anywhere . "melpa-stable")
        (inf-ruby . "melpa-stable")
        (js2-mode . "melpa-stable")
        (json-mode . "melpa-stable")
        (json-reformat . "melpa-stable")
        (json-snatcher . "melpa-stable")
        (kivy-mode . "melpa-stable")
        (less-css-mode . "melpa-stable")
        (logito . "melpa-stable")
        (lua-mode . "melpa-stable")
        (macrostep . "melpa-stable")
        (magit . "melpa-stable")
        (magit-popup . "melpa-stable")
        (magit-section . "melpa-stable")
        (makey . "melpa-stable")
        (markdown-mode . "melpa-stable")
        (marshal . "melpa-stable")
        (merlin . "melpa-stable")
        (operate-on-number . "melpa-stable")
        (pcache . "melpa-stable")
        (php-mode . "melpa-stable")
        (pkg-info . "melpa-stable")
        (pkgbuild-mode . "melpa-stable")
        (popup . "melpa-stable")
        (projectile . "melpa-stable")
        (protobuf-mode . "melpa-stable")
        (puppet-mode . "melpa-stable")
        (pythonic . "melpa-stable")
        (queue . "gnu")
        (rich-minority . "melpa-stable")
        (ruby-tools . "melpa-stable")
        (s . "melpa-stable")
        (sass-mode . "melpa-stable")
        (sbt-mode . "melpa-stable")
        (scala-mode . "melpa-stable")
        (scss-mode . "melpa-stable")
        (slim-mode . "melpa-stable")
        (slime . "melpa-stable")
        (smart-mode-line . "melpa-stable")
        (smartparens . "melpa-stable")
        (smartrep . "melpa-stable")
        (smex . "melpa-stable")
        (spinner . "gnu")
        (stylus-mode . "melpa-stable")
        (swift-mode . "melpa-stable")
        (thrift . "melpa-stable")
        (tuareg . "melpa-stable")
        (utop . "melpa-stable")
        (volatile-highlights . "melpa-stable")
        (web-mode . "melpa-stable")
        (which-key . "melpa-stable")
        (with-editor . "melpa-stable")
        (yaml-mode . "melpa-stable")
        (yasnippet . "melpa-stable")
        (zenburn-theme . "melpa-stable")
        (zop-to-char . "melpa-stable")))

(defvar x-packages
  '(ace-window
    ag
    avy
    anzu
    browse-kill-ring
    crux
    discover-my-major
    diff-hl
    diminish
    easy-kill
    editorconfig
    epl
    expand-region
    flycheck
    gist
    git-timemachine
    git-modes
    guru-mode
    hl-todo
    imenu-anywhere
    projectile
    magit
    move-text
    nlinum
    operate-on-number
    smartparens
    smartrep
    super-save
    undo-tree
    volatile-highlights
    which-key
    zenburn-theme
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(require 'cl-lib)

(defun x-packages-installed-p ()
  "Check if all packages in `x-packages' are installed."
  (cl-every #'package-installed-p x-packages))

(defun x-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package x-packages)
    (add-to-list 'x-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun x-require-packages (packages)
  "Ensure PACKAGES are installed.

Missing packages are installed automatically."
  (mapc #'x-require-package packages))

(defun x-install-packages ()
  "Install all packages listed in `x-packages'."
  (unless (x-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "EmacsX is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (x-require-packages x-packages)))

;; run package installation
(x-install-packages)

(defun x-list-foreign-packages ()
  "Browse third-party packages not bundled with EmacsX.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `x-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-set-difference package-activated-list x-packages)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; same with adoc-mode
(when (package-installed-p 'adoc-mode)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

;; and pkgbuild-mode
(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

(provide 'x-init-deps)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; x-init-deps.el ends here
