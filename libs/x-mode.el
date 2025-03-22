;;; x-mode.el --- Minor mode to encapsulate its extensions.

;;; Commentary:
;; A minor mode defining a local keymap, plus a menu.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'easymenu)
(require 'imenu-anywhere)
(require 'crux)

(defvar x-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'crux-open-with)
    (define-key map (kbd "C-c g") 'x-google)
    (define-key map (kbd "C-c G") 'github-browse-file)
    (define-key map (kbd "C-c y") 'x-youtube)
    (define-key map (kbd "C-c U") 'crux-find-user-init-file)
    (define-key map (kbd "C-c I") 'crux-find-user-custom-file)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    (define-key map (kbd "C-c i") 'imenu-anywhere)
    (define-key map (kbd "C-c j") 'avy-goto-word-or-subword-1)
    (define-key map (kbd "s-.") 'avy-goto-word-or-subword-1)
    (define-key map (kbd "C-c f") 'recentf-open-files)
    (define-key map (kbd "C-M-\\") 'crux-indent-defun)
    (define-key map (kbd "C-c P") 'x-package-upgrade-all)
    (define-key map (kbd "C-M-z") 'crux-indent-defun)
    (define-key map (kbd "C-c s") 'crux-swap-windows)
    (define-key map (kbd "C-c D") 'crux-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'crux-visit-term-buffer)
    (define-key map (kbd "C-c k") 'crux-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'crux-find-user-init-file)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    (define-key map (kbd "C-c e") 'crux-eval-and-replace)
    (define-key map (kbd "C-c C-r") 'ivy-resume)
    (define-key map (kbd "C-c w") 'x-cleanup-buffer)
    (define-key map (kbd "C-x C-b") 'ibuffer)
    map)
  "Keymap for X mode.")

(defun x-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun x-youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))

(defun x-package-upgrade-all ()
  "Refresh and upgrade all packages."
  (interactive)
  (package-refresh-contents)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute 'no-query))

(defun x-cleanup-buffer ()
  "Indent and remove trailing whitespace from the current buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max)))

;; define minor mode
(define-minor-mode x-mode
  "Minor mode to consolidate EmacsX extensions.

\\{x-mode-map}"
  :lighter " X"
  :keymap x-mode-map
  :global t)

(easy-menu-define x-mode-menu x-mode-map
  "Menu for EmacsX mode."
  '("X"
    ("Files"
     ["Open with..." x-open-with]
     ["Delete file and buffer" crux-delete-file-and-buffer]
     ["Rename buffer and file" crux-rename-buffer-and-file]
     ["Find user init file" crux-find-user-init-file]
     ["Find custom file" crux-find-user-custom-file]
     ["Find shell init file" crux-find-shell-init-file])
    ("Buffers"
     ["Clean up buffer or region" x-cleanup-buffer]
     ["Kill other buffers" crux-kill-other-buffers])
    ("Editing"
     ["Go to beginning of line" crux-move-beginning-of-line]
     ["Kill line backwards" crux-kill-line-backwards]
     ["Kill whole line" crux-kill-whole-line]
     ["Indent defun" crux-indent-defun]
     ["Duplicate line or region" crux-duplicate-current-line-or-region]
     ["Duplicate and comment line or region" crux-duplicate-and-comment-current-line-or-region]
     ["Indent rigidly and copy to clipboard" crux-indent-rigidly-and-copy-to-clipboard]
     ["Insert empty line below" crux-smart-open-line]
     ["Insert empty line above" crux-smart-open-line-above]
     ["Move line up" move-text-up]
     ["Move line down" move-text-down]
     ["Replace selection with clipboard contents" crux-replace-in-defun]
     ["Eval and replace" crux-eval-and-replace])
    ("Windows"
     ["Swap windows" crux-swap-windows])
    ("General"
     ["Visit term buffer" crux-visit-term-buffer]
     ["Search in Google" x-google]
     ["View URL" browse-url-at-point])))

(provide 'x-mode)
;;; x-mode.el ends here
