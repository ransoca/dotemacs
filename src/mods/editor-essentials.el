;;; MOD-editor-essentials.el --- Enhanced core editing experience.

;;; Commentary:
;; Refinements of the core editing experience.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(MOD:require-packages 'anzu
                      'browse-kill-ring
                      'consult
                      'company-mode
                      'editorconfig
                      'eglot
                      'expand-region
                      'embark
                      'embark-consult
                      'flycheck
                      'flycheck-pos-tip
                      'grizzl
                      'hydra
                      'marginalia
                      'multiple-cursors
                      'neotree
                      'orderless
		      'project
                      'projectile
                      'rainbow-delimiters
                      'smartparens
                      'tree-sitter
                      'tree-sitter-langs
                      'undo-tree
                      'vertico)

(defvar MOD:--auto-save-timer nil
  "Timer used to periodically auto-save buffers.")

;; Smart auto-save using the save-hook approach recommended in the
;; crux package (we install it automatically). This auto-save
;; is inspired by safeguard, a very helpful auto-save package
;; that unfortunately is being actively maintained.
(defun MOD:--auto-save-command ()
  "Save the current buffer if appropriate."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defun MOD:--auto-save (enable?)
  "Toggle auto-save all buffers with ENABLE? once in a while."
  (if enable?
      (unless MOD:--auto-save-timer
        (setq MOD:--auto-save-timer
              (run-with-idle-timer 3 t #'MOD:--auto-save-command)))
    (when MOD:--auto-save-timer
      (cancel-timer MOD:--auto-save-timer)
      (setq MOD:--auto-save-timer nil))))

(defun MOD:--wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(defun MOD:--expand-to-word-and-multiple-cursors (word)
  "Duplicates cursor over WORD provided!"
  (interactive "p")
  (if (region-active-p)
      (mc/mark-next-like-this word)
    (er/mark-word)))

(defmacro MOD:--move-back-horizontal-after (&rest code)
  `(let ((horizontal-position (current-column)))
     (progn
       ,@code
       (move-to-column horizontal-position))))

(defun MOD:--comment-or-uncomment-line-or-region ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (MOD:--move-back-horizontal-after
     (comment-or-uncomment-region (line-beginning-position) (line-end-position))
     (forward-line 1))))

(defun MOD:--duplicate-line ()
  (interactive)
  (MOD:--move-back-horizontal-after
   (move-beginning-of-line 1)
   (kill-line)
   (yank)
   (open-line 1)
   (forward-line 1)
   (yank)))

(defun MOD:--grizzl-recentf ()
  (interactive)
  (require 'grizzl)
  (let ((file (grizzl-completing-read "Recent: " (grizzl-make-index (reverse recentf-list)))))
    (when file
      (find-file file))))

(MOD:eval-after-init
 (global-set-key (kbd "s-+") 'text-scale-adjust)
 (global-set-key (kbd "s-=") 'text-scale-adjust)
 (global-set-key (kbd "s--") 'text-scale-adjust)
 (global-set-key (kbd "s-0") 'text-scale-adjust)

 (global-set-key (kbd "s-f") 'isearch-forward)
 (global-set-key (kbd "s-F") 'isearch-backward)

 (global-set-key (kbd "s-x") 'kill-region)
 (global-set-key (kbd "s-c") 'copy-region-as-kill)
 (global-set-key (kbd "s-v") 'yank)

 (when (and (eq system-type 'darwin) (display-graphic-p))
   (setq mac-command-modifier 'super)
   (setq mac-option-modifier 'meta))

 ;; Death to the tabs!  However, tabs historically indent to the next
 ;; 8-character offset; specifying anything else will cause *mass*
 ;; confusion, as it will change the appearance of every existing file.
 ;; In some cases (python), even worse -- it will change the semantics
 ;; (meaning) of the program.
 ;; Emacs modes typically provide a standard means to change the
 ;; indentation width -- eg. c-basic-offset: use that to adjust your
 ;; personal indentation width, while maintaining the style (and
 ;; meaning) of any files you load.
 (setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
 (setq-default tab-width 4)            ;; but maintain correct appearance

 ;; newline at end of file
 (setq require-final-newline t)

 ;; smart tab behavior - indent or complete
 (setq tab-always-indent 'complete)

 ;; disable annoying blink-matching-paren
 (setq blink-matching-paren nil)

 ;; (setq ido-enable-flex-matching t)
 ;; (setq ido-everywhere t)
 ;; (ido-mode 1)

 (set-default 'imenu-auto-rescan t)

 ;; editor config
 (global-display-line-numbers-mode)
 ;; highlight the current line
 (global-hl-line-mode +1)
 (setq display-line-numbers-type 'relative)  ; relative numbering

 (tool-bar-mode -1)
 (delete-selection-mode t)
 (fset 'yes-or-no-p 'y-or-n-p)
 (scroll-bar-mode -1)

 ;; store all backup and auto-save files in the respective directories
 (let ((backup-directory (expand-file-name "backups/" user-emacs-directory)))
   (setq backup-directory-alist `(("." . , backup-directory)))
   (unless (file-exists-p backup-directory) (make-directory backup-directory)))

 (let ((auto-save-directory (expand-file-name "autosaves/" user-emacs-directory)))
   (setq auto-save-file-name-transforms `((".*" , auto-save-directory t)))
   (unless (file-exists-p auto-save-directory) (make-directory auto-save-directory)))

 (MOD:--auto-save t)
 ;; revert buffers automatically when underlying files are changed externally
 (global-auto-revert-mode t)
 (add-hook 'before-save-hook 'delete-trailing-whitespace)

 (require 'saveplace)
 ;; save-place remembers your location in a file when saving files
 (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
 ;; activate it for all buffers
 (save-place-mode 1)

 ;; savehist keeps track of some history
 (require 'savehist)
 (setq savehist-additional-variables
       ;; search entries
       '(search-ring regexp-search-ring)
       ;; save every minute
       savehist-autosave-interval 60
       ;; keep the home clean
       savehist-file (expand-file-name "savehist" user-emacs-directory))
 (savehist-mode +1)

 ;; save recent files
 (require 'recentf)
 ;; (setq recentf-save-file (expand-file-name "recentf" x:save-file-dir))
 (setq recentf-max-saved-items 500)
 (setq recentf-max-menu-items 15)
 ;; disable recentf-cleanup on Emacs start, because it can cause
 ;; problems with remote files
 (setq recentf-auto-cleanup 'never)

 (run-with-timer 0 60 'recentf-save-list)

 (global-set-key (kbd "C-c f") 'MOD:--grizzl-recentf)

 (recentf-mode +1)

 ;; (add-to-list 'recentf-exclude 'MOD:--recentf-exclude-p)

 ;; hippie expand is dabbrev expand on steroids
 (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                          try-expand-dabbrev-all-buffers
                                          try-expand-dabbrev-from-kill
                                          try-complete-file-name-partially
                                          try-complete-file-name
                                          try-expand-all-abbrevs
                                          try-expand-list
                                          try-expand-line
                                          try-complete-lisp-symbol-partially
                                          try-complete-lisp-symbol))

 (global-set-key (kbd "RET") 'newline-and-indent)
 (global-set-key (kbd "s-]") 'switch-to-next-buffer)
 (global-set-key (kbd "s-[") 'switch-to-prev-buffer)
 (global-set-key (kbd "s-D") 'MOD:--duplicate-line)
 (global-set-key (kbd "M-;") 'MOD:--comment-or-uncomment-line-or-region)

 (define-key minibuffer-local-map (kbd "DEL") 'backward-kill-word)

 (require 'windmove)

 ;; custom keybindings
 (global-set-key (kbd "M-s-<left>") 'windmove-left)
 (global-set-key (kbd "M-s-<right>") 'windmove-right)
 (global-set-key (kbd "M-s-<up>") 'windmove-up)
 (global-set-key (kbd "M-s-<down>") 'windmove-down)

 ;; packages config

 (require 'anzu)
 (global-anzu-mode +1)
 (setq anzu-cons-mode-line-p nil)  ; Show in mode line
 (setq anzu-search-threshold 0)    ; Always show counter
 (global-set-key (kbd "M-%") 'anzu-query-replace)
 (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

 (add-hook 'org-mode-hook (lambda () (anzu-mode -1)))

 (require 'browse-kill-ring)
 (setq browse-kill-ring-separator "\n\n")

 ;; replace default yank-pop
 (global-set-key (kbd "C-x y") 'browse-kill-ring)

 (require 'consult)
 (setq consult-preview-key "M-.")

 ;; C-c bindings in `mode-specific-map'
 (global-set-key (kbd "C-c M-x") 'consult-mode-command)
 (global-set-key (kbd "C-c h") 'consult-history)
 (global-set-key (kbd "C-c k") 'consult-kmacro)
 (global-set-key (kbd "C-c m") 'consult-man)
 (global-set-key (kbd "C-c i") 'consult-info)

 ;; C-x bindings in `ctl-x-map'
 (global-set-key (kbd "C-x M-:") 'consult-complex-command)     ; orig. repeat-complex-command
 (global-set-key (kbd "C-x b") 'consult-buffer)                ; orig. switch-to-buffer
 (global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window) ; orig. switch-to-buffer-other-window
 (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
 (global-set-key (kbd "C-x t b") 'consult-buffer-other-tab)    ; orig. switch-to-buffer-other-tab
 (global-set-key (kbd "C-x r b") 'consult-bookmark)            ; orig. bookmark-jump
 (global-set-key (kbd "C-x p b") 'consult-project-buffer)      ; orig. project-switch-to-buffer

 ;; Custom M-# bindings for fast register access
 (global-set-key (kbd "M-#") 'consult-register-load)
 (global-set-key (kbd "M-'") 'consult-register-store)          ; orig. abbrev-prefix-mark
 (global-set-key (kbd "C-M-#") 'consult-register)

 ;; Other custom bindings
 (global-set-key (kbd "M-y") 'consult-yank-pop)                ; orig. yank-pop


 ;; M-g bindings in `goto-map'
 (global-unset-key (kbd "M-g"))
 (global-set-key (kbd "M-g e") 'consult-compile-error)
 (global-set-key (kbd "M-g f") 'consult-flymake)               ; alt. consult-flycheck
 (global-set-key (kbd "M-g g") 'consult-goto-line)             ; orig. goto-line
 (global-set-key (kbd "M-g M-g") 'consult-goto-line)           ; orig. goto-line
 (global-set-key (kbd "M-g o") 'consult-outline)               ; alt. consult-org-heading
 (global-set-key (kbd "M-g m") 'consult-mark)
 (global-set-key (kbd "M-g k") 'consult-global-mark)
 (global-set-key (kbd "M-g i") 'consult-imenu)
 (global-set-key (kbd "M-g I") 'consult-imenu-multi)

 ;; M-s bindings in `search-map'
 (global-set-key (kbd "M-s d") 'consult-find)                  ; alt. consult-fd
 (global-set-key (kbd "M-s c") 'consult-locate)
 (global-set-key (kbd "M-s g") 'consult-grep)
 (global-set-key (kbd "M-s G") 'consult-git-grep)
 (global-set-key (kbd "M-s r") 'consult-ripgrep)
 (global-set-key (kbd "M-s l") 'consult-line)
 (global-set-key (kbd "M-s L") 'consult-line-multi)
 (global-set-key (kbd "M-s k") 'consult-keep-lines)
 (global-set-key (kbd "M-s u") 'consult-focus-lines)
 (global-set-key (kbd "M-s e") 'consult-isearch-history) ; I-search integration

 (define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history) ; orig. isearch-edit-string
 (define-key isearch-mode-map (kbd "M-s l") 'consult-line) ; needed to detect isearch
 (define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi) ; needed to detect isearch

 (define-key minibuffer-local-map (kbd "M-s") 'consult-history) ; orig. next-matching-history-element
 (define-key minibuffer-local-map (kbd "M-r") 'consult-history) ; orig. previous-matching-history-element

 (require 'company)
 (global-company-mode)
 (setq company-idle-delay 0.5)  ; Delay before showing suggestions
 (setq company-minimum-prefix-length 3)  ; Minimum chars to trigger
 (add-hook 'org-mode-hook (lambda () (company-mode -1)))

 ;; use settings from .editorconfig file when present
 (require 'editorconfig)
 (editorconfig-mode 1)

 (require 'eglot)

 (require 'embark)
 (require 'embark-consult)

 ;; enable embark
 (global-set-key (kbd "C-.") 'embark-act)
 (global-set-key (kbd "C-;") 'embark-dwim)
 (global-set-key (kbd "C-h B") 'embark-bindings)

 (require 'flycheck)
 (global-flycheck-mode)
 (add-hook 'org-mode-hook (lambda () (flycheck-mode -1)))
 (setq flycheck-hbighlighting-mode 'lines)  ; Highlight lines

 (require 'flycheck-pos-tip)
 (flycheck-pos-tip-mode)
 (setq flycheck-tip-timeout 1.0)  ; Delay before showing tooltip
 (setq flycheck-tip-show-tip-at-point t)  ; Show on hover

 (require 'marginalia)
 (setq marginalia-annotators '(marginalia-annotators-heavy
                               marginalia-annotators-light
                               nil))
 (marginalia-mode +1)

 (require 'projectile)

 (setq projectile-enable-caching t)
 (setq projectile-globally-ignored-files '("*.elc" "*.pyc"))
 (setq projectile-globally-ignored-directories '(".git" ".svn"))
 (setq projectile-cache-file (expand-file-name  "projectile.cache" MOD:cache-dir))
 (setq projectile-bookmark-cache-file (expand-file-name "bookmarks.cache" MOD:cache-dir))

 (projectile-mode +1)

 (global-set-key (kbd "s-t") 'projectile-find-file)
 (global-set-key (kbd "s-g") 'projectile-grep)
 (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

 (setq mc/always-run-for-all t)
 (global-set-key (kbd "s-d") 'MOD:--expand-to-word-and-multiple-cursors)

 (require 'rainbow-delimiters)
 (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

 (require 'expand-region)
 (global-set-key (kbd "C-=") 'er/expand-region)

 (require 'neotree)
 (setq neo-theme 'ascii)
 (global-set-key (kbd "s-1") 'neotree-toggle)

 (require 'orderless)
 (setq completion-styles '(orderless))
 (setq completion-category-overrides '((file (styles . (partial-completion)))))

 ;; smart pairing for all
 (require 'smartparens-config)
 (setq sp-autoskip-closing-pair 'always)
 (setq sp-hybrid-kill-entire-symbol nil)
 (setq sp-highlight-pair-overlay nil)  ;; Disable overlay highlighting

 (sp-use-smartparens-bindings)

 (define-key prog-mode-map (kbd "M-(") (MOD:--wrap-with "("))

 (smartparens-global-mode t)  ; enable globally
 (show-smartparens-global-mode +1)

 (require 'undo-tree)

 (setq undo-tree-auto-save-history nil)

 (global-unset-key (kbd "s-u"))
 (global-set-key (kbd "s-u") 'undo-tree-visualize)

 (global-unset-key (kbd "s-z"))
 (global-set-key (kbd "s-z") 'undo-tree-undo)

 (global-unset-key (kbd "s-Z"))
 (global-set-key (kbd "s-Z") 'undo-tree-redo)

 (global-undo-tree-mode)

 (require 'vertico)
 (setq vertico-cycle t)

 (define-key vertico-map (kbd "C-j") 'vertico-next)
 (define-key vertico-map (kbd "C-k") 'vertico-previous)
 (define-key vertico-map (kbd "C-f") 'vertico-exit)

 (vertico-mode +1)

 (require 'tree-sitter)
 (require 'tree-sitter-langs)

 (global-tree-sitter-mode))

(provide 'MOD-editor-essentials)
;;; MOD-editor-essentials.el ends here
