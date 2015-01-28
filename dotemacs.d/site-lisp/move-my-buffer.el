;;; move-my-buffer.el --- A simple script to move the current buffer

;; Copyright (C) 2015  Michael Wall

;; Author: Michael Wall
;; Version: 0.1

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This is a simple script to move the current buffer to another
;; window.  It works by calling `bury-buffer' on the the current
;; window, moving to another window and then calling `unbury-buffer'.
;;
;; If moving to another window fails for any reason, `unbury-buffer'
;; is still called and there is no apparent effect except any message
;; the failed function put in the *Message* buffer.
;;
;; The script uses only functions that are autoloaded, like the
;; `windmove-left' function, so no other package is required.
;;
;; While not required, this script is designed to work with
;; `winner-mode' and the `windmove-default-keybindings'.
;;
;; Inspiration for this script comes from buffer-move.el by Lucas
;; Bonnet and the built-in windmove.el.  Unlike buffer-move,
;; this script does now swap the contents of the move buffers.
;;
;; Example usage:
;;
;; After this file is in your load path, put the following in your
;; startup.
;;
;; (require 'move-my-buffer)
;;
;; You can then setup keybindings manually with something like
;;
;; (global-set-key (kbd "S-M-<left>") 'move-my-buffer-left)
;; (global-set-key (kbd "S-M-<right>") 'move-my-buffer-right)
;; (global-set-key (kbd "S-M-<up>") 'move-my-buffer-up)
;; (global-set-key (kbd "S-M-<down>") 'move-my-buffer-down)
;; (global-set-key (kbd "S-M-<F8>") 'move-my-buffer-out)
;;
;; or use the provided function to accomplish same thing, which
;; also binds to "S-M-<direction>"
;;
;; (move-my-move-my-buffer-default-keybindings)

(defun move-my-buffer (windmove-fn)
  "Moves the current buffer to the window target of the `windmove-fn'.
Example: `(move-my-buffer 'windmove-left)'."
  (progn
    (bury-buffer)
    (unwind-protect
        (funcall windmove-fn)
      (unbury-buffer))))

(defun move-my-buffer-left ()
  "Move buffer to the left"
  (interactive)
  (move-my-buffer 'windmove-left))
(defun move-my-buffer-right ()
  "Move buffer to the right"
  (interactive)
  (move-my-buffer 'windmove-right))
(defun move-my-buffer-up ()
  "Move buffer to the up"
  (interactive)
  (move-my-buffer 'windmove-up))
(defun move-my-buffer-down ()
  "Move buffer to the down"
  (interactive)
  (move-my-buffer 'windmove-down))
(defun move-my-buffer-out ()
  "Make a new frame with the current buffer"
  (interactive)
  (move-my-buffer 'make-frame))

(defun mmb-flatten (lst)
  "Flatten a nested list, from the book GNU Emacs Extensions"
  (if (null lst)
      nil
    (if (listp (car lst))
        (append (mmb-flatten (car lst))
                (mmb-flatten (cdr lst)))
      (cons (car lst)
            (mmb-flatten (cdr lst))))))

(defun move-my-buffer-default-keybindings (&optional modifier)
  "Set up keybindings for `move-my-buffer'.
Keybindings are of the form MODIFIER-{left,right,up,down}.
Default MODIFIER is 'shift 'meta.

Modeled after `windmove-default-keybindings' from `windmove.el'"
  (interactive)
  (unless modifier (setq modifier (list 'shift 'meta)))
  (global-set-key (vector (mmb-flatten (list modifier 'left)))  'move-my-buffer-left)
  (global-set-key (vector (mmb-flatten (list modifier 'right))) 'move-my-buffer-right)
  (global-set-key (vector (mmb-flatten (list modifier 'up)))    'move-my-buffer-up)
  (global-set-key (vector (mmb-flatten (list modifier 'down)))  'move-my-buffer-down)
  (global-set-key (vector (mmb-flatten (list modifier 'f8)))  'move-my-buffer-out))

(provide 'move-my-buffer)
;;; move-my-buffer.el ends here
