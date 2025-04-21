;;; MOD-font-fira-code.el --- For Fira Code.

;;; Commentary:
;; Sets default font to fira code.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(MOD:require-packages 'diminish
                      'fira-code-mode)

(MOD:eval-after-init
 (set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

 (require 'fira-code-mode)
 (setq fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures that we don't want

 (global-fira-code-mode +1)

 (require 'diminish)
 (diminish 'fira-code-mode))

(provide 'MOD-font-fira-code)

;;; MOD-font-fira-code.el ends here
