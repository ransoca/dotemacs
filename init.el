;;; init.el --- Initialization.

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

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define directory structure
(defvar x:dir (file-name-directory load-file-name)
  "The root directory for initialization.")

(defvar x:init-dir (expand-file-name "init.d" x:dir)
   "The root directory for configuration.")

(add-to-list 'load-path x:init-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; (message "[X] Loading X's core modules...")

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

;;; init.el ends here

