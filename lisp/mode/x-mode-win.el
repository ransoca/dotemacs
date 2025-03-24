;;; x-mode-win-nt.el --- MS Windows NT specific setup.

;;; Commentary:
;; Additional setup that's useful when running editor in MS Windows.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

;; Teach Emacs how to interpret various modifier keys
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; Git setup (assuming you've installed Git for Windows)
(when (file-exists-p "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH"))))

;; needed for arc-mode (it allows you to open archives in Emacs)
(if (file-exists-p "C:/Program Files/7-Zip")
    (add-to-list 'exec-path "C:/Program Files/7-Zip")
  (message "7-Zip not found. It's a good idea to install it."))

(provide 'x-mode-win-nt)
;;; x-mode-win-nt.el ends here
