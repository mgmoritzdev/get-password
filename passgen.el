;; use passgen to copy a new password to kill-ring
;; it's also possible to use the internal function passgen--generate
;; to select which kind of characters you want in your password and
;; how many.
;;
;; Usage examples:
;; basic:
;; (passgen 16)
;; => returns the password with all kinds of chars programmed
;;
;; using internal function passgen--generate
;; (passgen--generate 16 10 1 1 1)
;; returns the the password with at least 1 of each kind of chars

(defvar passgen--special-chars "@%%+/!#$^?:.~`-_.")
(defvar passgen--alpha-lower "abcdefghijklmnopqrstuvwxyz")
(defvar passgen--alpha-upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar passgen--numeric "0123456789")

(defun passgen (&optional arg)
  "Generates a strong password with `LENGTH` characters."
  (interactive "p")
  (if (<= arg 1)
      (let ((length (read-number "Type the password length: ")))
        (passgen--generate length 1 1 1 1))
    (passgen--generate arg 1 1 1 1)))

(defun passgen--generate (length &optional alpha-lower alpha-upper numeric special-chars)
  (if (< length (+ (if (numberp alpha-lower) alpha-lower 0)
                   (if (numberp alpha-upper) alpha-upper 0)
                   (if (numberp numeric) numeric 0)
                   (if (numberp special-chars) special-chars 0)))
      (error "The `length' must be greater or equal the sum of the rest of the parms"))
  (let ((char-pool "")
        (result))
    (if alpha-lower
        (progn
          (setq result (append result (passgen--get 'alpha-lower)))
          (setq char-pool (concat char-pool passgen--alpha-lower))))

    (if alpha-upper
        (progn
          (setq result (append result (passgen--get 'alpha-upper)))
          (setq char-pool (concat char-pool passgen--alpha-upper))))

    (if numeric
        (progn
          (setq result (append result (passgen--get 'numeric)))
          (setq char-pool (concat char-pool passgen--numeric))))

    (if special-chars
        (progn
          (setq result (append result (passgen--get 'special-chars)))
          (setq char-pool (concat char-pool passgen--special-chars))))

    (if (= 0 (length char-pool))
        (setq char-pool passgen--alpha-lower))
    (setq remainder (- length (length result)))
    (dotimes (i remainder)
      (let ((index (% (abs (random)) (length char-pool))))
        (setq result (append result `(,(substring char-pool index (1+ index)))))))
    (setq result (shuffle result))
    (kill-new (mapconcat 'concat result ""))))

(defun passgen--get (symbol)
  (let* ((result)
         (symbol-value (symbol-value symbol))
         (symbol-name (symbol-name symbol))
         (symbol-charlist (symbol-value (intern (concat "passgen--" symbol-name)))))
    (if (numberp symbol-value)
        (dotimes (i symbol-value)
          (let ((index (% (abs (random)) (length symbol-charlist))))
            (setq result (append result `(,(substring symbol-charlist index (1+ index))))))))
    result))

;; From http://kitchingroup.cheme.cmu.edu/blog/2014/09/06/Randomize-a-list-in-Emacs/
(defun swap (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))


(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap LIST i j)))
  LIST)
