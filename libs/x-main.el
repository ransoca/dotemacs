;;; x-main.el --- Initialization.

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

(defvar x:main-libs-dir (expand-file-name "main" x:libs-dir)
  "The home for inital functionality.")

;; Add directories to load path
(add-to-list 'load-path x:main-libs-dir)

(message "[X] Loading main modules...")

;; initializing
(require 'x-main-deps)
(require 'x-main-defs)

(provide 'x-main)
;;; x-main.el ends here
