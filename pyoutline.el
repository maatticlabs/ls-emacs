;-*-unibyte: t;-*-

;;;;++
;;;; Name
;;;;    pyoutline
;;;;
;;;; Purpose
;;;;    Stolen from a comp.lang.python posting by laura Creighton
;;;;    <lac@strakt.com>
;;;;    Message-id: <200205121906.g4CJ6brW005045@theraft.strakt.com>
;;;;
;;;; Revision Dates
;;;;    13-May-2002 (CT) Creation
;;;;    ««revision-date»»···
;;;;--
;;;;
(provide 'pyoutline)

(defvar outline-start-hidden t "Start outline hidden")

(defun outline-setup (regexp)
  "Setup outline mode"
  (defvar outline-toggle-all-flag nil "toggle all flag")
  (defvar cpos_save nil "current cursor position")
  (outline-minor-mode)
  (setq outline-regexp regexp)
  (define-key outline-minor-mode-map "\C-c\C-e" 'outline-toggle-entry)
  (define-key outline-minor-mode-map "\C-c\C-a" 'outline-toggle-all)
  (if outline-start-hidden
      (hide-body))

  (defun outline-toggle-entry () (interactive)
    "Toggle outline hiding for the entry under the cursor"
    (if (progn
          (setq cpos_save (point))
          (end-of-line)
          (get-char-property (point) 'invisible))
        (progn
          (show-subtree)
          (goto-char cpos_save))
      (progn
        (hide-leaves)
        (goto-char cpos_save))))

  (defun outline-toggle-all () (interactive)
    "Toggle outline hiding for the entire file"
    (if outline-toggle-all-flag
        (progn
          (setq outline-toggle-all-flag nil)
          (hide-body))
      (progn
        (setq outline-toggle-all-flag t)
        (show-all))))
)

(defun python-outline () (interactive)
  "Python outline mode"
  (python-mode)
  (outline-setup "^class \\|[   ]*def \\|^#"))

(defun texi-outline () (interactive)
  "Texinfo outline mode"
  (texinfo-mode)
  (outline-setup "^@chap\\|@\\(sub\\)*section"))
