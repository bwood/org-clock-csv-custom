;; https://stackoverflow.com/a/15725437/844985

(eval-after-load "org-clock-csv"
  '(defun org-clock-csv ()
     "Overridden org-clock-csv."
     (interactive)
       (message "I override!")))

(defun bw-test ()
  "A test function."
  (interactive)
    (message "I override!"))

