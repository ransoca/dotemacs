;;; x-macos.el --- Emacs X: macOS specific settings.


;;; Commentary:

;; Some macOS specific stuff.

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

;; On macOS Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(x-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; I prefer cmd key as meta
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; Curse Lion and its sudden but inevitable fullscreen mode!
;; I'd still prefer that the fullscreen resizing be more graceful, but
;; this works fairly well.
(setq ns-use-native-fullscreen nil)

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; Fullscreen mode - 3 options
;; (setq mac-cocoa-fullscreen-use-native nil)  ;; classic

;; map keys for moving around between windows
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-l") 'windmove-right)

(provide 'x-macos)
;;; x-macos.el ends here
