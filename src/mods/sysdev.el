;;; MOD-sysdev.el --- Enhanced system development experience.
;;; Commentary:
;; Refinements of the system development experience.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(MOD:require-packages 'cargo
                      'flycheck-rust
                      'go-mode
                      'go-test
                      'rust-mode
                      'rustic)

(defun MOD:--go-mode-defaults ()
  "Default mode hook for Go prog mode."
  ;; stop whitespace being highlighted
  (whitespace-toggle-options '(tabs))
  ;; CamelCase aware editing operations
  (subword-mode +1)
  (local-set-key (kbd "M-s-a") 'go-test-current-project)
  (local-set-key (kbd "M-s-f") 'go-test-current-file)
  (local-set-key (kbd "M-s-t") 'go-test-current-test)
  (local-set-key (kbd "M-s-r") 'go-run)
  (local-set-key (kbd "M-s-d") 'godoc-at-point))

(defun MOD:--project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun MOD:--eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(defun MOD:--eglot-organize-imports-before-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (call-interactively 'eglot-code-action-organize-imports))
            nil t))

(defun MOD:--rust-mode-defaults ()
  "Defaults for Rust mode."
  (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
  (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))

(defun MOD:--rustic-mode-defaults ()
  "Enable auto-saving in rustic-mode buffers."
  ;; format on save
  (setq rust-format-on-save t)
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil))
  ;; CamelCase aware editing operations
  (subword-mode +1))

(MOD:eval-after-init
 (require 'project)

 (cl-defmethod project-root ((project (head go-module)))
   (cdr project))

 (add-hook 'project-find-functions #'MOD:--project-find-go-module)

 (require 'go-mode)
 (require 'eglot)

 (add-hook 'go-mode-hook 'eglot-ensure)
 (add-hook 'go-mode-hook 'MOD:--go-mode-defaults)
 (add-hook 'go-mode-hook #'MOD:--eglot-format-buffer-before-save)
 (add-hook 'go-mode-hook #'MOD:--eglot-organize-imports-before-save)

 (setq rust-mode-treesitter-derive t)

 (add-hook 'rust-mode-hook 'cargo-minor-mode)
 (add-hook 'rust-mode-hook #'tree-sitter-mode)
 (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
 (add-hook 'rust-mode-hook 'MOD:--rust-mode-defaults)

 (setq rustic-analyzer-command '("~/.bin/rusta"))
 (setq rustic-lsp-client 'eglot)

 (add-hook 'rustic-mode-hook 'MOD:--rustic-mode-defaults)

 (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(provide 'MOD-sysdev)
;;; MOD-sysdev.el ends here
