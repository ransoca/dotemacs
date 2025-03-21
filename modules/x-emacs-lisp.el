;;; x-emacs-lisp.el --- Emacs Prelude: Nice config for Elisp programming.


;;; Commentary:

;; Nice config for Elisp Programming.

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

(require 'x-lisp)
(require 'crux)

(x-require-packages '(elisp-slime-nav rainbow-mode))

(defun x-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p x-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun x-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'x-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun x-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun x-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'x-lisp-coding-hook)
  (eldoc-mode +1)
  (x-recompile-elc-on-save)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (x-conditional-emacs-lisp-checker))

(setq x-emacs-lisp-mode-hook 'x-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'x-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; ielm is an interactive Emacs Lisp shell
(defun x-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'x-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(setq x-ielm-mode-hook 'x-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'x-ielm-mode-hook)))

(with-eval-after-load "elisp-slime-nav"
  (diminish 'elisp-slime-nav-mode))
(with-eval-after-load "rainbow-mode"
  (diminish 'rainbow-mode))
(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

(with-eval-after-load "ielm"
  (define-key ielm-map (kbd "M-(") (x-wrap-with "("))
  (define-key ielm-map (kbd "M-\"") (x-wrap-with "\"")))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(provide 'x-emacs-lisp)

;;; x-emacs-lisp.el ends here
