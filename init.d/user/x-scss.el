;;; x-scss.el --- Emacs Prelude: scss support


;;; Commentary:

;; Some basic configuration for scss-mode.

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

(require 'x-css)
(x-require-packages '(scss-mode))

;; turn off annoying auto-compile on save
(setq scss-compile-at-save nil)

(defun x-scss-mode-defaults ()
  (x-css-mode-defaults))

(setq x-scss-mode-hook 'x-scss-mode-defaults)

(add-hook 'scss-mode-hook (lambda () (run-hooks 'x-scss-mode-hook)))

(provide 'x-scss)
;;; x-scss.el ends here
