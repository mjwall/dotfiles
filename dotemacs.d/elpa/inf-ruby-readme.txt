inf-ruby.el provides a REPL buffer connected to an IRB subprocess.

If you're installing manually, you'll need to:
* drop the file somewhere on your load path (perhaps ~/.emacs.d)
* Add the following lines to your .emacs file:
   (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
   (autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
   (eval-after-load 'ruby-mode
     '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))
