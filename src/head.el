;;; head.el --- Initialization.

;;; Commentary:
;; Initialization of the editor config.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

;; always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; disable both startup screen and message
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; set default font to 15pt equivalent
(set-face-attribute 'default nil :height 140)

; disable audible beep and modify visual bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(defvar MOD:cache-dir (expand-file-name "caches" user-emacs-directory))
(unless (file-exists-p MOD:cache-dir)
  (make-directory MOD:cache-dir))

(add-to-list 'exec-path "~/.bin")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "PWD/var")

(setq MOD:init-packs '())
(defvar MOD:init-hooks nil)

(defun MOD:require-packages (&rest packages)
  (setq MOD:init-packs
    (append MOD:init-packs packages)))

(defun MOD:customize-package (name &rest details)
  (setq el-get-sources
        (cons (append (list :name name) details) el-get-sources)))

(defmacro MOD:eval-after-init (&rest body)
  `(add-hook 'MOD:init-hooks
     (lambda ()
       ,@body)))

(add-to-list 'load-path "PWD/var/deps")

;;; head.el ends here
