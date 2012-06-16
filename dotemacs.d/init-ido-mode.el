;; ido-mode is like magic pixie dust!
;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
;        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess ;nil
        ido-auto-merge-work-directories-length 0
        ido-default-buffer-method 'selected-window
        ido-max-prospects 10
        ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
          "_region_" " output\\*$" "^TAGS$" "^\*Ido")
        ido-ignore-directories
        '("\\`auto/" "\\`auto-save-list/" "\\`backups/" "\\`semanticdb/" "\\`target/" "\\`\\.git/" "\\`\\.svn/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
        ido-ignore-files
        '("\\`auto/" "\\.prv/" "_region_" "\\.class/"  "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")))

;; taken from http://github.com/superbobry/emacs/blob/master/rc/emacs-rc-defuns.el
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


;; Display ido results vertically, rather than horizontally
;; from http://www.emacswiki.org/emacs/InteractivelyDoThings#toc17
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; update keybindings so up and down move next with vertical results
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)))

(provide 'init-ido-mode)

