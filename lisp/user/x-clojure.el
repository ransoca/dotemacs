;;; x-clojure.el --- Clojure programming configuration.

;;; Commentary:
;; A basic setup for Clojure programming based on clojure-mode
;; and CIDER.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'x-lisp)
(x-require-packages '(clojure-mode cider))

(with-eval-after-load 'clojure-mode
  (defun x-clojure-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'x-lisp-coding-hook))

  (setq x-clojure-mode-hook 'x-clojure-mode-defaults)

  (add-hook 'clojure-mode-hook (lambda ()
                                 (run-hooks 'x-clojure-mode-hook))))

(with-eval-after-load 'cider
  (setq nrepl-log-messages t)

  (add-hook 'cider-mode-hook 'eldoc-mode)

  (defun x-cider-repl-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'x-interactive-lisp-coding-hook))

  (setq x-cider-repl-mode-hook 'x-cider-repl-mode-defaults)

  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (run-hooks 'x-cider-repl-mode-hook))))

(provide 'x-clojure)

;;; x-clojure.el ends here
