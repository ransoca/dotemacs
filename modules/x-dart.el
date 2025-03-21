;;; x-dart.el --- Emacs Prelude: Dart programming configuration.


;;; Commentary:

;; Basic configuration for programming in Dart.

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

(require 'x-lsp)
(x-require-packages '(lsp-dart))

(with-eval-after-load 'lsp-dart
  (add-hook 'dart-mode-hook #'lsp))

(with-eval-after-load 'dart-mode
  (defun x-dart-mode-defaults ()

    (setq dap-launch-configuration-providers  '(dap-debug-template-configurations-provider))

    ;; Add to default dart-mode key bindings
    (lsp-dart-define-key "s o" #'lsp-dart-show-outline)
    (lsp-dart-define-key "s f" #'lsp-dart-show-flutter-outline)
    (lsp-dart-dap-setup))

  (setq x-dart-mode-hook 'x-dart-mode-defaults)

  (add-hook 'dart-mode-hook (lambda ()
                            (run-hooks 'x-dart-mode-hook))))

(provide 'x-dart)

;;; x-dart.el ends here
