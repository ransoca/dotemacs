;;; x-mode-mac.el --- Darwin/macOS specific settings.

;;; Commentary:
;; Some Apple's Darwin/macOS specific stuff.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

;; On macOS Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(x-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; I prefer cmd key as meta
;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)

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

(provide 'x-mode-mac)
;;; x-mode-mac.el ends here
