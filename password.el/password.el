(defun moritz/get-password-and-run-callback (callback)
  "Get password and run callback"
  (let ((keys (with-temp-buffer
                (insert-file-contents "~/.gnupg/ptmkeys.json.gpg")
                (buffer-string)
                (json-read))))
    (let ((keys-helm-source
           `((name . "Select a key: ")
             (candidates . ,(mapcar '(lambda (element) `(,(cdr (assoc 'name element)) . ,element))
                                    keys))
             (action . callback))))
      (helm :sources '(keys-helm-source)))))

(defun moritz/copy-password-to-kill-ring ()
  "Get password and copy it to kill-ring"
  (interactive)
  (moritz/get-password-and-run-callback
   'moritz/parse-password-and-copy-to-kill-ring))

(defun moritz/insert-password ()
  "Get password and insert it in the current position"
  (interactive)
  (moritz/get-password-and-run-callback
   'moritz/parse-password-and-insert))

(defun moritz/parse-password-and-copy-to-kill-ring (candidate)
  (kill-new (cdr (assoc 'password candidate))))

(defun moritz/parse-password-and-insert (candidate)
  (insert (cdr (assoc 'password candidate))))
