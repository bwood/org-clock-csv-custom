;; Override the autoloaded function org-clock-csv.
;; https://stackoverflow.com/a/15725437/844985
(eval-after-load "org-clock-csv"
  '(defun org-clock-csv ()
     "Overridden org-clock-csv."
     (interactive)
       (message "I override!")))

