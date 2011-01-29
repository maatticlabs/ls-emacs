;-*- coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency swing-kartei-telefon.el swi_ktlf.el
(provide 'swing-kartei-phone)

(defvar swing-kartei:phone:directory "/swing/tkartei/")
(defvar swing-kartei:phone:file-name "phone")

(defun swing-kartei:phone:extract-entry 
           (limit phone-macro-name get-leading)
  (let ((leading (funcall get-leading limit))
        phone-val
        key-val
        head
       )
    (while (setq phone-val 
                 (swing-kartei:field+option-values limit phone-macro-name)
           )
      (set-buffer dst-buf)
      (setq head (point))
      (insert leading "\t" 
              (if (stringp phone-val)
                  phone-val
                (concat 
                    (if (stringp (setq key-val (nth 1 phone-val)))
                        (concat key-val "\t")
                      "\t"
                    )
                    (car phone-val)
                )
              )
              "\n"
      )
      (subst-char-in-region head (1- (point)) ?\n ?  t)
      (set-buffer src-buf)
      (setq swing-kartei:n (1+ swing-kartei:n))
      (if (equal (% swing-kartei:n 10) 0) (message "... %d" swing-kartei:n))
    )
  )
; swing-kartei:phone:extract-entry
)

(defun swing-kartei:phone:extract
           (phone-kartei-dir  phone-kartei-name 
            kartei-dir        kartei-name
            phone-macro-name  leading
           )
  (save-window-excursion
    (let ((swing-kartei:n 0)
          (gc-cons-threshold 100000); all other values take much more time!!!
          (src-buf (swing-kartei:get-file-buffer 
                        kartei-dir kartei-name "kartei"
                   )
          )
          (dst-buf (swing-kartei:get-file-buffer       
                        phone-kartei-dir phone-kartei-name "summary"
                   )
          )
          p
         )
      (switch-to-buffer src-buf)
      (goto-char (point-min))
      (while (swing-kartei:entry:find ".+" 'quiet)
        (setq p (match-end 0))
        (swing-kartei:phone:extract-entry p phone-macro-name leading)
        (goto-char p)
      )
      (switch-to-buffer dst-buf)
      (goto-char 1)
      (swing-kartei:remove-tex-markup)
      (shell-command-on-region 1 (point-max) "sort -b -d -f" t nil)
      (message "Phone-Kartei contains %d entries" swing-kartei:n)
    )
  )
; swing-kartei:phone:extract
)

(defun swing-kartei:phone:person-leading (limit)
  (let ((leading "")
        field
       )
    (save-excursion (setq field (swing-kartei:field-value limit "DefName")))
    (if field (setq leading field))
    (save-excursion (setq field (swing-kartei:field-value limit "DefVorname")))
    (if field (setq leading (concat leading " " field)))
    (save-excursion (setq field (swing-kartei:field-value limit "DefTitel")))
    (if field (setq leading (concat leading ", " field)))
    (if (not (equal leading ""))
        leading
    )
  )
; swing-kartei:phone:person-leading
)

(defun swing-kartei:phone:extract-person 
           (&optional phone-kartei-dir  phone-kartei-name 
                      person-kartei-dir person-kartei-name
           )
  "Extract phone numbers from person kartei"
  (interactive)
  (swing-kartei:phone:extract 
       (or phone-kartei-dir   swing-kartei:phone:directory)
       (or phone-kartei-name  swing-kartei:phone:file-name)
       (or person-kartei-dir  swing-kartei:person:directory)
       (or person-kartei-name swing-kartei:person:file-name)
       "DefTelefon"     
       'swing-kartei:phone:person-leading
  )
)


(defun swing-kartei:phone:firma-leading (limit)
  (let ((leading "")
        abbr
        name
       )
    (save-excursion 
      (setq abbr (swing-kartei:field-value limit "DefFirmaKurz"))
    )
    (if abbr (setq leading abbr))
    (save-excursion 
      (setq name (swing-kartei:field-value limit "DefFirmenName"))
    )
    (if name 
        (if abbr
            (if (string-match (regexp-quote abbr) name)
                (setq leading name)
              (setq leading (concat leading " " name))
            )
          (setq leading name)
        )
    )
    (if (not (equal leading ""))
        leading
    )
  )
; swing-kartei:phone:firma-leading
)

(defun swing-kartei:phone:extract-firma 
           (&optional phone-kartei-dir  phone-kartei-name 
                      firma-kartei-dir firma-kartei-name
           )
  "Extract phone numbers from firma kartei"
  (interactive)
  (swing-kartei:phone:extract 
       (or phone-kartei-dir   swing-kartei:phone:directory)
       (or phone-kartei-name  swing-kartei:phone:file-name)
       (or firma-kartei-dir   swing-kartei:firma:directory)
       (or firma-kartei-name  swing-kartei:firma:file-name)
       "DefFirmenTelefon"     
       'swing-kartei:phone:firma-leading
  )
)

(defun swing-kartei:phone:generate 
           (&optional phone-kartei-dir   phone-kartei-name 
                      person-kartei-dir  person-kartei-name
                      firma-kartei-dir   firma-kartei-name
           )
  "Generate new phone kartei from person and firma kartei."
  (interactive)
  (let ((dst-buf (swing-kartei:get-file-buffer       
                    (or phone-kartei-dir  swing-kartei:phone:directory)
                    (or phone-kartei-name swing-kartei:phone:file-name)
                    "summary"
                 )
        )
       )
    (save-window-excursion
      (switch-to-buffer dst-buf)
      (erase-buffer)
      (swing-kartei:phone:extract-person 
           phone-kartei-dir  phone-kartei-name 
           person-kartei-dir person-kartei-name
      )
      (swing-kartei:phone:extract-firma 
           phone-kartei-dir phone-kartei-name 
           firma-kartei-dir firma-kartei-name
      )
    )
  )
; swing-kartei:phone:generate
)
