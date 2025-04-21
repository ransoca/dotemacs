;;; MOD-webdev.el --- Enhanced web devlopment experience.

;;; Commentary:
;; Refinements of the web development experience.

;;; License:
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(MOD:require-packages 'coffee-mode
                      'rainbow-mode
                      'scss-mode
                      'js2-mode
                      'json-mode
                      'rjsx-mode
                      'tide
                      'web-mode)

(defun MOD:--css-mode-defaults ()
  "Defaults for CSS mode."
  (rainbow-mode +1))

(defun MOD:--scss-mode-defaults ()
  "Defaults for SCSS mode."
  (MOD:--css-mode-defaults))

(defun MOD:--js-mode-defaults ()
  "Defaults for JS mode."
  ;; electric-layout-mode doesn't play nice with smartparens
  (setq-local electric-layout-rules '((?\; . after)))
  (setq mode-name "JS")
  (js2-imenu-extras-mode +1)
  (subword-mode +1))

(defun MOD:--jsx-mode-defaults ()
  "Defaults for JSX mode."
  (setq mode-name "JSX"))

(defun MOD:--ts-mode-defaults ()
  "Setup Tide mode for TypeScript."
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (company-mode +1)
  (eldoc-mode +1))

(MOD:eval-after-init
 (require 'coffee-mode)

 (setq coffee-tab-width 2)

;; (require 'css-mode)
 (require 'rainbow-mode)

 (setq css-indent-offset 2)

 (add-hook 'css-mode-hook 'MOD:--css-mode-defaults)

 (require 'scss-mode)

 (setq scss-compile-at-save nil)

 (add-hook 'scss-mode-hook 'MOD:--scss-mode-defaults)

 (require 'js2-mode)

 (add-to-list 'auto-mode-alist '("\\.js\\'"     . js2-mode))
 (add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js2-mode))
 (add-to-list 'auto-mode-alist '("\\.pac\\'"    . js2-mode))
 (add-to-list 'interpreter-mode-alist '("node"  . js2-mode))

 (setq js-indent-level 2)

 (add-hook 'js2-mode-hook 'smartparens-strict-mode)
 (add-hook 'js2-mode-hook 'show-paren-mode)
 (add-hook 'js2-mode-hook 'MOD:--js-mode-defaults)

 (require 'rjsx-mode)

 (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
 (add-hook 'rjsx-mode-hook 'MOD:--jsx-mode-defaults)

 (require 'web-mode)

 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
 (add-to-list 'auto-mode-alist
              '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

 ;; make web-mode play nice with smartparens
 (setq web-mode-enable-auto-pairing nil)

 (require 'smartparens)

 (sp-with-modes '(web-mode)
  (sp-local-pair "%" "%"
                 :unless '(sp-in-string-p)
                 :post-handlers '(((lambda (&rest _ignored)
                                     (just-one-space)
                                     (save-excursion (insert " ")))
                                   "SPC" "=" "#")))
  (sp-local-tag "%" "<% "  " %>")
  (sp-local-tag "=" "<%= " " %>")
  (sp-local-tag "#" "<%# " " %>"))

 (require 'tide)

 (add-hook 'typescript-hook 'MOD:--ts-mode-defaults)
 (add-hook 'tsx-mode-hook 'MOD:--ts-mode-defaults)
 (add-hook 'before-save 'tide-format-before-save))

(provide 'MOD-webdev)

;;; MOD-webdev.el ends here
