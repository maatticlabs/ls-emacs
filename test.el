(defun set-property ()
  (interactive)
  (add-text-properties (point) (mark)
      (list 'insert-in-front-hooks '(test-insert-property))))

(defun test-insert--property (head tail)
  (message (format "Buffer around point looks like: '%s'"
   (buffer-substring (- head 2) (+ tail 2)))))