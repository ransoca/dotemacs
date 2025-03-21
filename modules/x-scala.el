;;; x-scala.el --- Emacs Prelude: scala-mode configuration.


;;; Commentary:

;; Some basic support for the Scala programming language

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

(require 'x-programming)
(x-require-packages '(scala-mode lsp-mode))

(defun x-scala-mode-defaults ()
  (subword-mode +1)
  (lsp))

(setq x-scala-mode-hook 'x-scala-mode-defaults)

(add-hook 'scala-mode-hook (lambda ()
                             (run-hooks 'x-scala-mode-hook)))
(provide 'x-scala)

;;; x-scala.el ends here
