;;; MOD-appdev.el --- Enhanced application development experience.
;;; Commentary:
;; Refinements of the application development experience.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(MOD:require-packages 'markdown-mode
                      'python-mode
                      'python-black
                      'anaconda-mode
                      'company-anaconda
                      'ruby-mode
                      'inf-ruby
                      'robe-mode
                      'rspec-mode)

(defun MOD:--x-ruby-mode-defaults ()
  "Defaults for the Ruby mode."
  ;; Don't auto-insert encoding comments
  ;; Those are almost never needed in Ruby 2+
  (setq ruby-insert-encoding-magic-comment nil)
  (inf-ruby-minor-mode +1)
  ;; CamelCase aware editing operations
  (subword-mode +1))

(MOD:eval-after-init
 (require 'eglot)

 (require 'markdown-mode)
 (autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
 (add-to-list 'auto-mode-alist
              '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

 (autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
 (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

 (with-eval-after-load 'markdown-mode
   (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do))

 (require 'python-mode)
 ;; IPython REPL
 (setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i --simple-prompt")

 (add-to-list 'company-backends 'company-anaconda)

 (add-to-list 'eglot-server-programs
              '(python-mode . ("jedi-language-server")))

 (add-hook 'python-mode-hook 'eglot-ensure)
 (add-hook 'python-mode-hook 'anaconda-mode)
 (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
 (add-hook 'python-mode-hook 'python-black-on-save-mode-enable-dwim)

 (require 'ruby-mode)
 ;; inf ruby
 (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
 (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

 ;; robe mode
 (add-hook 'ruby-mode-hook 'robe-mode)
 (add-hook 'ruby-ts-mode-hook 'robe-mode)

 ;; rspec mode
 (require 'rspec-mode))

(provide 'MOD-appdev)
;;; MOD-appdev.el ends here
