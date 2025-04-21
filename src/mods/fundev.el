;;; MOD-fundev.el --- Enhanced funtional development experience.
;;; Commentary:
;; Refinements of the functional development experience.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(MOD:require-packages 'cider
                      'align-cljlet
                      'fill-column-indicator
                      'flycheck-clojure
                      'erlang-mode
                      'erlware-mode
                      'elixir
                      'alchemist
                      'haskell-mode)

(defcustom clojure-column-line nil
  "When non nil, puts a line at some character on clojure mode"
  :type 'integer
  :group 'clojure)

(defun custom-cider-shortcuts ()
  "Custom shortcuts for cider."
  (local-set-key (kbd "C-c ,") 'cider-test-run-tests)
  (local-set-key (kbd "C-c M-o") 'cider-repl-clear-buffer))

(defun custom-turn-on-fci-mode ()
  (when clojure-column-line
    (setq fci-rule-column clojure-column-line)
    (turn-on-fci-mode)))

(defun cider-format-buffer-back () (interactive)
       (let (p)
         (setq p (point))
         (cider-format-buffer)
         (goto-char p)))

(defun add-clj-format-before-save () (interactive)
       (add-hook 'before-save-hook
                 'cider-format-buffer-back
                 t t))

(defmacro clojure:save-before-running (function)
  `(defadvice ,function (before save-first activate)
     (save-buffer)))

(defmacro clojure:load-before-running (function)
  `(defadvice ,function (before save-first activate)
     (cider-load-buffer)))

(MOD:eval-after-init
 (require 'smartparens)

 ;; clojure
 (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
 (setq cider-auto-select-error-buffer t)
 (setq cider-repl-popup-stacktraces t)

 (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
 (add-hook 'clojure-mode-hook 'show-paren-mode)
 (add-hook 'clojure-mode-hook 'sp-use-paredit-bindings)
 (add-hook 'clojure-mode-hook (lambda () (cljr-add-keybindings-with-prefix "C-c C-m")))
 (add-hook 'clojure-mode-hook 'custom-cider-shortcuts)
 (add-hook 'clojure-mode-hook 'custom-turn-on-fci-mode)
 (add-hook 'clojure-mode-hook 'add-clj-format-before-save)

 (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
 (add-hook 'cider-repl-mode-hook 'show-paren-mode)

 (clojure:save-before-running cider-load-current-buffer)
 (clojure:load-before-running cider-test-run-tests)
 (clojure:load-before-running cider-test-rerun-tests)
 (clojure:load-before-running cider-test-run-test)

 ;; erlang
 (setq erlang-root-dir "/usr/homebrew/lib/erlang")
 (setq erlang-man-root-dir "/usr/homebrew/lib/erlang/man")

 (add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
 (add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

 ;; elixir

 (setq elixir-iex-command "/usr/homebrew/bin/iex")
 (setq elixir-compiler-command "/usr/homebrew/bin/elixirc")

 (add-hook 'elixir-mode-hook 'alchemist-mode)
 (add-hook 'elixir-mode-hook 'smartparens-strict-mode))

(provide 'MOD-fundev)
;;; MOD-fundev.el ends here
