;;; init.el --- Initialization.

;;; Commentary:
;; Initialization of the editor config..

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define directory structure
(defvar x:dir (file-name-directory load-file-name)
  "The root directory for initialization.")

(defvar x:libs-dir (expand-file-name "libs" x:dir)
   "The root directory for configuration.")

;; (defvar x-init:mods-dir (expand-file-name "mods.d" x-init:root-dir)
;;   "This directory houses all of the built-in X modules.")

;; (defvar x-init:libs-dir (expand-file-name "libs.d" x-init:root-dir)
;;   "This directory houses packages that are not yet available in ELPA (or MELPA).")

;; (defvar x-modules-file (expand-file-name "x-modules.el" x-personal-dir)
;;   "This file contains a list of modules that will be loaded by X.")

;; (defun x-init:-add-subfolders-to-load-path (parent-dir)
;;   "Add all level PARENT-DIR subdirs to the `load-path'."
;;   (dolist (f (directory-files parent-dir))
;;     (let ((name (expand-file-name f parent-dir)))
;;       (when (and (file-directory-p name)
;;                  (not (string-prefix-p "." f)))
;;         (add-to-list 'load-path name)
;;         (x-init:-add-subfolders-to-load-path name)))))

;; Add directories to load path
(add-to-list 'load-path x:libs-dir)
;; (add-to-list 'load-path x-init:mods-dir)
;; (add-to-list 'load-path x-init:libs-dir)
;; (x-init:-add-subfolders-to-load-path x-init:libs-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; (defvar x-init:user-dir (expand-file-name "user" x-init:dir)
;;   "This directory is for personal configuration.")

;; (defvar x-init:load-dir (expand-file-name "preload" x-personal-dir)
;;   "This directory is for your personal configuration, that you want loaded before X.")

;; (let (load-dir (expand-file-name "preload" x-personal-dir))
;;   (when (file-exists-p x-personal-preload-dir)
;;     (message "[X] Loading personal configuration files in %s..." x-personal-preload-dir)
;;     (mapc 'load (directory-files x-personal-preload-dir 't "^[^#\.].*el$"))))

;; preload the personal settings from `x-personal-preload-dir'


;; (message "[X] Loading X's core modules...")

;; initializing
(require 'x-main)

;; configuration
(require 'x-core)

;; modularization
(require 'x-mode)

;; macOS specific settings
;; (when (eq system-type 'darwin)
;;   (require 'x-macos))

;; Windows specific settings
;; (when (eq system-type 'windows-nt)
;;  (require 'x-windows))

;; (message "[X] Loading X's additional modules...")

;; the modules
;; (if (file-exists-p x-modules-file)
;;    (load x-modules-file)
;;  (load (expand-file-name "sample/x-modules.el" x-dir)))

;; config changes made through the customize UI will be stored here
;; (setq custom-file (expand-file-name "custom.el" x-personal-dir))

;; load the personal settings (this includes `custom-file')
;; (when (file-exists-p x-personal-dir)
;;   (message "[X] Loading personal configuration files in %s..." x-personal-dir)
;;   (mapc 'load (delete
;;                x-modules-file
;;                (directory-files x-personal-dir 't "^[^#\.].*\\.el$"))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
