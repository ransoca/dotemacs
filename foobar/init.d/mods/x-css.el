;;; x-css.el --- Emacs Prelude: css support


;;; Commentary:

;; Some basic configuration for css-mode.

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

(with-eval-after-load 'css-mode
  (x-require-packages '(rainbow-mode))

  (setq css-indent-offset 2)

  (defun x-css-mode-defaults ()
    (rainbow-mode +1)
    (run-hooks 'x-prog-mode-hook))

  (setq x-css-mode-hook 'x-css-mode-defaults)

  (add-hook 'css-mode-hook (lambda ()
                             (run-hooks 'x-css-mode-hook))))

(provide 'x-css)
;;; x-css.el ends here
