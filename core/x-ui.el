;;; x-ui.el --- Emacs X: UI optimizations and tweaks.


;;; Commentary:

;; UI configuration for a better Emacs experience.

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

;; get rid of menu, tool and scroll bars and other graphical widgets
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(set-fringe-mode 10)
(setq frame-resize-pixelwise t)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable non-selected window highlight
(setq-default cursor-in-non-selected-windows nil)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Emacs default color theme, a pleasing warm theme
(load-theme 'leuven t)

;; Setup default custom theme
(when (boundp 'x-theme)
  (load-theme x-theme t))

;; nice scrolling
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; show number of matches in search calls
(setq isearch-lazy-count t)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)
(setq uniquify-buffer-name-style 'forward)

;; Emoji: 😄, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'x-ui)

;;; x-ui.el ends here
