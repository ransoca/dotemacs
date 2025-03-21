;;; x-custom.el --- Emacs X: Custom.


;;; Commentary:

;; Refinements of the core editing experience in Emacs.

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

;; customize
(defgroup x nil
  "Emacs X configuration."
  :prefix "x-"
  :group 'convenience)

(defcustom x-auto-save nil
  "Non-nil values enable X's auto save."
  :type 'boolean
  :group 'x)

(defcustom x-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'x)

(defcustom x-whitespace t
  "Non-nil values enable X's whitespace visualization."
  :type 'boolean
  :group 'x)

(defcustom x-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `x-whitespace' is also enabled."
  :type 'boolean
  :group 'x)

(defcustom x-flyspell t
  "Non-nil values enable X's flyspell support."
  :type 'boolean
  :group 'x)

(defcustom x-user-init-file (expand-file-name "personal/"
                                               user-emacs-directory)
  "Path to your personal customization file.
X recommends you only put personal customizations in the
personal folder.  This variable allows you to specify a specific
folder as your personal customization folder.  This is
particularly useful if you have your EmacsX checkout as a
subdirectory of your regular `.emacs.d' folder."
  :type 'string
  :group 'x)

(defcustom x-user-init-path user-emacs-directory
  "Path where additional user configuration files reside."
  :type 'string
  :group 'x)

(defcustom x-completing-read-handlers
  '((execute-extended-command    . helm-smex)
    (dired-do-copy               . helm-read-file-name)
    (dired-do-rename             . helm-read-file-name)
    (dired-create-directory      . helm-read-file-name))
  "Special handlers for `completing-read'."
  :type 'alist
  :group 'x)

(defcustom x-theme 'zenburn
  "The default color theme, change this in your /personal/preload config."
  :type 'symbol
  :group 'x)

(defcustom x-shell (getenv "SHELL")
  "The default shell to run with `crux-visit-term-buffer'"
  :type 'string
  :group 'x)

(defcustom x-format-on-save t
  "Format the file with the relevant mode formatter before saving."
  :type 'boolean
  :group 'x)

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset, LaTeX requires setting
;; variables like LaTeX-indent-level, TeX-brace-indent-level, etc.
;; Changing the basic indentation offset, however, is sufficient
;; for most situations, hence x-indent-sensitive-modes:
(defcustom x-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'x)

(defcustom x-yank-indent-modes
  '(latex-mode LaTeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'x)

(defcustom x-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'x)

(defcustom x-scroll-margin 5
  "Controls how much margin to keep between the cursor and the window edge."
  :type 'number
  :group 'x)

(provide 'x-custom)

;;; x-custom.el ends here
