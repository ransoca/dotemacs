;;; MOD-colour-theme-solarize.el --- Colour theme solarized.

;;; Commentary:
;; Both dark and light Emacs color theme.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(MOD:require-packages 'solarize)

(MOD:eval-after-init
 (add-to-list 'custom-theme-load-path
              (expand-file-name "el-get/solarize" user-emacs-directory))

 (load-theme 'solarized t)

 (add-hook 'after-make-frame-functions
           (lambda (frame)
             (solarized-update-background-mode
              (if (display-graphic-p frame) 'light 'dark)
              (list frame))))

 (add-hook 'ns-system-appearance-change-functions
           #'solarized-update-background-mode))

(provide 'MOD-colour-theme-solarize)

;;; MOD-colour-theme-solarize.el ends here
