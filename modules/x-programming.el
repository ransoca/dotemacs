;;; x-programming.el --- Emacs Prelude: prog-mode configuration


;;; Commentary:

;; Some basic prog-mode configuration and programming related utilities.

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

(defun x-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO in source code
(require 'hl-todo)
(global-hl-todo-mode 1)

;; in Emacs 24 programming major modes generally derive from a common
;; mode named prog-mode; for others, we'll arrange for our mode
;; defaults function to run x-prog-mode-hook directly.  To
;; augment and/or counteract these defaults your own function
;; to x-prog-mode-hook, using:
;;     (add-hook 'x-prog-mode-hook 'my-prog-mode-defaults t)
;; (the final optional t sets the *append* argument)

;; smart curly braces
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))

;; enlist a more liberal guru
(setq guru-warn-only t)

(defun x-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (and (executable-find ispell-program-name)
             x-flyspell)
    (flyspell-prog-mode))
  (when x-guru
    (guru-mode +1)
    (diminish 'guru-mode))
  (smartparens-mode +1)
  (x-enable-whitespace)
  (x-local-comment-auto-fill))

(setq x-prog-mode-hook 'x-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'x-prog-mode-hook)))

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'x-programming)
;;; x-programming.el ends here
