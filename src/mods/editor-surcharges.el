;;; MOD-editor-surcharges.el --- Enhanced core editing experience.
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
(MOD:require-packages 'diff-hl
                      'magit
                      'magit-delta)

(MOD:eval-after-init
 ;; diff-hl
 (require 'diff-hl)
 (global-diff-hl-mode +1)
 (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
 (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

 ;; magit
 (global-set-key (kbd "C-c g") 'magit-status)

 ;; magit delta
 (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))

(provide 'MOD-editor-surcharges)
;;; MOD-editor-surcharges.el ends here
