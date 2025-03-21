;;; x-core.el --- Emacs X: Core X functions.


;;; Commentary:

;; Core X helper functions.

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

(defun x-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

;; Some basic setup
;; (add-to-list 'default-frame-alist '(font . "Fira Code"))

;; Smart auto-save using the save-hook approach recommended in the
;; crux package (X installs it automatically).  X's auto-save
;; is inspired by safeguard, a very helpful auto-save package
;; that unfortunately is being actively maintained.
(defun x-auto-save-command ()
  "Save the current buffer if appropriate."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defvar x-auto-save-timer nil
  "Timer used to periodically auto-save buffers.")

(defun x-enable-auto-save ()
  "Auto-save all buffers once in a while."
  (unless x-auto-save-timer
    (setq x-auto-save-timer
          (run-with-idle-timer 3 t #'x-auto-save-command))))

(defun x-disable-auto-save ()
  "Disable auto-save."
  (when x-auto-save-timer
    (cancel-timer x-auto-save-timer)
    (setq x-auto-save-timer nil)))

(when x-auto-save
  (x-enable-auto-save))

(defvar x-tips
  '("Explore the X menu to find out about some of X extensions to Emacs."
    "A lot of key combinations have been added to the original Emacs keybindings to make your life easier. See the key-binding section of the docs to learn more."
    "If you disable a built-in mode line, choose another.")
  "A list of X tips.")

(defun x-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun x-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(provide 'x-core)
;;; x-core.el ends here
