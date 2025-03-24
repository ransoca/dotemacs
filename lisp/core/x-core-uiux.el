;;; x-core-uiux.el --- UI/UX optimizations and tweaks.

;;; Commentary:
;; UI/UX configuration for a better editor experience.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
(global-nlinum-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Emacs default color theme, a pleasing warm theme
(load-theme 'leuven t)

;; Setup default custom theme
(when (boundp 'x:theme)
  (load-theme x:theme t))

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

(provide 'x-core-uiux)

;;; x-ui.el ends here
