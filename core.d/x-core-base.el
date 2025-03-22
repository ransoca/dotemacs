;;; x-core-base.el --- Foundational Functionality.

;;; Commentary:
;; All basic helper functions.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defun x-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

;; Smart auto-save using the save-hook approach recommended in the
;; crux package (X installs it automatically).  X's auto-save
;; is inspired by safeguard, a very helpful auto-save package
;; that unfortunately is being actively maintained.
(defun x-auto-save-command ()
  "Save the current buffer if appropriate."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defvar x-auto-save-timer nil
  "Timer used to periodically auto-save buffers.")

(defun x-enable-auto-save ()
  "Auto-save all buffers once in a while."
  (unless x-auto-save-timer
    (setq x-auto-save-timer
          (run-with-idle-timer 3 t #'x-auto-save-command))))

(defun x-disable-auto-save ()
  "Disable auto-save."
  (when x-auto-save-timer
    (cancel-timer x-auto-save-timer)
    (setq x-auto-save-timer nil)))

(when x:auto-save
  (x-enable-auto-save))

(defvar x-tips
  '("Explore the X menu to find out about some of X extensions to Emacs."
    "A lot of key combinations have been added to the original Emacs keybindings to make your life easier. See the key-binding section of the docs to learn more."
    "If you disable a built-in mode line, choose another.")
  "A list of X tips.")

(defun x-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun x-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(provide 'x-core-base)
;;; x-core-base.el ends here
