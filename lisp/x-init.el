;;; x-init.el --- Initialization.

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

;; booting
(require 'x-boot)

;; initializing
(require 'x-main)

;; configuration
(require 'x-core)

;; modularization
(require 'x-mode)

(x-mode t)

;; customization
(require 'x-user)

(provide 'x-init)
;;; x-init.el ends here