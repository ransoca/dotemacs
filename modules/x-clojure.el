;;; x-clojure.el --- Emacs Prelude: Clojure programming configuration.


;;; Commentary:

;; A basic setup for Clojure programming based on clojure-mode
;; and CIDER.

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
(x-require-packages '(clojure-mode cider))

(with-eval-after-load 'clojure-mode
  (defun x-clojure-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'x-lisp-coding-hook))

  (setq x-clojure-mode-hook 'x-clojure-mode-defaults)

  (add-hook 'clojure-mode-hook (lambda ()
                                 (run-hooks 'x-clojure-mode-hook))))

(with-eval-after-load 'cider
  (setq nrepl-log-messages t)

  (add-hook 'cider-mode-hook 'eldoc-mode)

  (defun x-cider-repl-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'x-interactive-lisp-coding-hook))

  (setq x-cider-repl-mode-hook 'x-cider-repl-mode-defaults)

  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (run-hooks 'x-cider-repl-mode-hook))))

(provide 'x-clojure)

;;; x-clojure.el ends here
