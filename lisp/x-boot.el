;;; x-boot.el --- A listing of user modules to load on startup

;;; Commentary:
;; For convenience the modules are grouped in several categories.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defvar x:boot-dir (expand-file-name "boot" x:dir)
  "The home for inital functionality.")

;; Add directories to load path
(add-to-list 'load-path x:boot-dir)

(defun x:-add-subdirs-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
         (x:-add-subdirs-to-load-path name)))))

(x:-add-subdirs-to-load-path x:boot-dir)

(defvar x:-pre-load-boot-dir (expand-file-name "pre-load" x:boot-dir))

(when (file-exists-p x:-pre-load-boot-dir)
  (message "[X] Loading personal configuration files in %s..." x:-pre-load-boot-dir)
  (mapc 'load (directory-files x:-pre-load-boot-dir 't "^[^#\.].*el$")))

(provide 'x-boot)
;;; x-boot.el ends here
