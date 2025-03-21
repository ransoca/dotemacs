;; Always load newest byte code
(setq load-prefer-newer t)

;; Define X's directory structure
(defvar x-dir (file-name-directory load-file-name)
  "The root dir of the Emacs X distribution.")
(defvar x-core-dir (expand-file-name "core" x-dir)
  "The home of X's core functionality.")
(defvar x-modules-dir (expand-file-name  "modules" x-dir)
  "This directory houses all of the built-in X modules.")
(defvar x-personal-dir (expand-file-name "personal" x-dir)
  "This directory is for your personal configuration.

Users of EmacsX are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by X.")
(defvar x-personal-preload-dir (expand-file-name "preload" x-personal-dir)
  "This directory is for your personal configuration, that you want loaded before X.")
(defvar x-vendor-dir (expand-file-name "vendor" x-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar x-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar x-modules-file (expand-file-name "x-modules.el" x-personal-dir)
  "This file contains a list of modules that will be loaded by X.")

(unless (file-exists-p x-savefile-dir)
  (make-directory x-savefile-dir))

(defun x-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (x-add-subfolders-to-load-path name)))))

;; add X's directories to Emacs's `load-path'
(add-to-list 'load-path x-core-dir)
(add-to-list 'load-path x-modules-dir)
(add-to-list 'load-path x-vendor-dir)
(x-add-subfolders-to-load-path x-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `x-personal-preload-dir'
(when (file-exists-p x-personal-preload-dir)
  (message "[X] Loading personal configuration files in %s..." x-personal-preload-dir)
  (mapc 'load (directory-files x-personal-preload-dir 't "^[^#\.].*el$")))

(message "[X] Loading X's core modules...")

;; load the core stuff
(require 'x-packages)
(require 'x-custom)  ;; Needs to be loaded before core, editor and ui
(require 'x-ui)
(require 'x-core)
(require 'x-mode)
(require 'x-editor)
(require 'x-global-keybindings)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'x-macos))

;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'x-windows))

(message "[X] Loading X's additional modules...")

;; the modules
(if (file-exists-p x-modules-file)
    (load x-modules-file)
  (load (expand-file-name "sample/x-modules.el" x-dir)))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" x-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p x-personal-dir)
  (message "[X] Loading personal configuration files in %s..." x-personal-dir)
  (mapc 'load (delete
               x-modules-file
               (directory-files x-personal-dir 't "^[^#\.].*\\.el$"))))

;;; init.el ends here
