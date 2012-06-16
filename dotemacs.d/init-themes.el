(require 'color-theme-autoloads)

;; elpa installed themes
(autoload 'color-theme-zenburn "color-theme-zenburn"
  "A low contrast color theme" t)
(autoload 'color-theme-twilight "color-theme-twilight"
  "A dark color theme" t)
(autoload 'color-theme-ir-black "color-theme-ir-black"
  "A dark color theme" t)
(autoload 'color-theme-railscasts "color-theme-railscasts"
  "Translation of railscasts theme for Textmate" t)

;; themes in site-lisp/themes
(autoload 'color-theme-tango-2 "color-theme-tango-2"
  ;; https://raw.github.com/wfarr/color-theme-tango-2/master/color-theme-tango-2.el
  "A dark color theme" t)

(autoload 'color-theme-zen-and-art "color-theme-zen-and-art"
  ;; https://raw.github.com/irfn/zen-and-art/master/zen-and-art.el
  "A dark color theme" t)

(autoload 'color-theme-subdued "color-theme-subdued"
  ;; http://jblevins.org/git/misc.git/plain/color-theme-subdued.el
  "A dark color theme" t)

(autoload 'color-theme-vivid-chalk "color-theme-vivid-chalk"
  ;;https://raw.github.com/flavorjones/emacs.d/master/color-theme-vivid-chalk.el
  "A dark color theme" t)

;;   https://raw.github.com/purcell/color-theme-sanityinc/master/color-theme-sanityinc.el
(require 'color-theme-sanityinc)
;;(color-theme-sanityinc-dark)
(color-theme-vivid-chalk)

(provide 'init-themes)
