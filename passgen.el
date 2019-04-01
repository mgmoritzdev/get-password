(defvar passgen--special-chars "@%%+\\/'!#$^?:.(){}[]~`-_.")
(defvar passgen--alpha-lower "abcdefghijklmnopqrstuvwxyz")
(defvar passgen--alpha-upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar passgen--numeric "0123456789")

(defun passgen ()
  "Generates a strong password with `LENGTH` characters."
  (interactive)
  (let ((length (read-number "Type the password length: ")))
    (passgen--generate length t t t t)))

(defun passgen--generate (length &optional alpha-lower alpha-upper numeric special-chars)
  (let ((alpha-lower (if (boundp alpha-lower)
                         alpha-lower
                       t))
        (alpha-upper (if (boundp alpha-upper)
                         alpha-upper
                       t))
        (special-chars (if (boundp special-chars)
                           special-chars
                         t))
        (numeric (if (boundp numeric)
                     numeric
                   t))
        (string-pool "")
        (result))
    (if alpha-lower
        (setq string-pool (concat string-pool passgen--alpha-lower)))
    (if alpha-upper
        (setq string-pool (concat string-pool passgen--alpha-upper)))
    (if numeric
        (setq string-pool (concat string-pool passgen--numeric)))
    (if special-chars
        (setq string-pool (concat string-pool passgen--special-chars)))
    (dotimes (i length)
      (let ((index (% (abs (random)) (length string-pool))))
        (setq result (append result `(,(substring string-pool index (1+ index)))))))
    (kill-new (mapconcat 'concat result ""))))
