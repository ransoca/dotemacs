;;; x-core.el --- Initialization.

;;; Commentary:
;; Initialization of the editor config..

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defvar x:core-libs-dir (expand-file-name "core" x:libs-dir)
   "The home of configuration functionality.")

;; Add directories to load path
(add-to-list 'load-path x:core-libs-dir)

(message "[X] Loading core modules...")

;; intializing
(require 'x-core-uiux)
(require 'x-core-base)
(require 'x-core-hack)
(require 'x-core-keys)

(provide 'x-core)
;;; x-core.el ends here
