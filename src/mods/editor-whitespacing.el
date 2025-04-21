;; MOD-editor-whitespacing.el --- Enhanced whitespacing experience for editor
;;; Commentary:
;; Refinements of the whitespacing for editing experience.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(MOD:require-packages 'diminish
                      'whitespace-cleanup-mode
                      'unicode-whitespace)

(MOD:eval-after-init
 (setq-default truncate-lines nil)
 (setq-default global-visual-line-mode nil)

 (require 'unicode-whitespace)
 (unicode-whitespace-setup 'subdued-faces)

 (global-whitespace-mode +1)

 (require 'whitespace-cleanup-mode)
 (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)

 (require 'diminish)
 (diminish 'whitespace-cleanup-mode))

(provide 'MOD-editor-whitespacing)
;;; MOD-editor-whitespacing.el ends here
