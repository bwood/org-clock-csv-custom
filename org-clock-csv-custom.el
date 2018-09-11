;; Customize rows
;; requires this in custom-set-variables:
;;   ='(org-clock-csv-row-fmt (quote org-clock-csv-default-row-fmt-custom))=
;;   = '(org-clock-csv-header "start,category,end,task")=
(defun org-clock-csv-default-row-fmt-custom (plist)
  "Default row formatting function."
  (mapconcat #'identity
             (list (plist-get plist ':start)
	           (org-clock-csv--escape (plist-get plist ':task))
		   (plist-get plist ':duration)     	           
		   (org-clock-csv--escape (plist-get plist ':wpsbilling)))
		   ","))

(eval-after-load "org-clock-csv--parse-element"
(defun org-clock-csv--parse-element (element)
  "Ingest clock ELEMENT and produces a plist of its relevant
properties."
  (when (and (equal (org-element-type element) 'clock)
             ;; Only ingest closed, inactive clock elements.
             (equal (org-element-property :status element) 'closed)
             (equal (org-element-property
                     :type (org-element-property :value element))
                    'inactive-range))
    (let* ((timestamp (org-element-property :value element))
           (headlines (org-clock-csv--find-headlines element)) ;; Finds the headlines ancestor lineage containing the clock element.
           (headlines-values (mapcar (lambda (h) (org-element-property :raw-value h)) headlines ))
           (task-headline (car headlines)) ;; The first headline containing this clock element.
           (task (car headlines-values))
           (parents (reverse (cdr headlines-values)))
           (effort (org-element-property :EFFORT task-headline))
           ;; TODO: Handle tag inheritance, respecting the value of
           ;; `org-tags-exclude-from-inheritance'.
           (tags (mapconcat #'identity
                            (org-element-property :tags task-headline) ":"))
           (ishabit (when (equal "habit" (org-element-property
                                          :STYLE task-headline))
                      "t"))
           (category (org-clock-csv--find-category task-headline))
           (start (format "%d-%s-%s"
                          (org-element-property :year-start timestamp)
                          (org-clock-csv--pad
                           (org-element-property :month-start timestamp))
                          (org-clock-csv--pad
                           (org-element-property :day-start timestamp))
                          (org-clock-csv--pad
                           (org-element-property :minute-start timestamp))))
           (end (format "%d-%s-%s %s:%s"
                        (org-element-property :year-end timestamp)
                        (org-clock-csv--pad
                         (org-element-property :month-end timestamp))
                        (org-clock-csv--pad
                         (org-element-property :day-end timestamp))
                        (org-clock-csv--pad
                         (org-element-property :hour-end timestamp))
                        (org-clock-csv--pad
                         (org-element-property :minute-end timestamp))))
	   (duration (format "%s.%s"
			     (nth 0 (s-split ":" (org-element-property :duration element)))
			     (org-clock-csv-custom--fractional-hours
			      (org-element-property :duration element))))
	   (wpsbilling (org-clock-csv-custom--find-wps-property task-headline)))
      
      (list :task task
            :parents parents
            :category category
            :start start
            :end end
	    :duration duration
	    :wpsbilling wpsbilling
            :effort effort
            :ishabit ishabit
            :tags tags)))))

(defun org-clock-csv-custom--fractional-hours (str)
  "Convert minutes to a fraction of 1 hour."
  (nth 1 (s-split "\\." (format "%0.2f" (/ (float(string-to-number (nth 1 (s-split ":" str)))) 60)))))

;; Based on org-clock-csv.el: org-clock-csv--find-category
(defun org-clock-csv-custom--find-wps-property (element)
  "Find the WPS billing property of a headline ELEMENT, optionally recursing
upwards until one is found.

Returns an empty string if no WPS billing property is found."
  (let ((wpsbilling (org-element-property :WPS element))
        (current element)
        (curlvl  (org-element-property :level element)))
    ;; If the headline does not have a wps property, recurse upwards
    ;; through the parent headlines, checking if there is a wps property
    ;; property in any of them.
    (while (not wpsbilling)
      (setq current (if (equal curlvl 1)
                        (org-element-lineage current)
                      (org-element-lineage current '(headline)))
            curlvl (- curlvl 1))
      (setq wpsbilling (org-element-property :WPS current))
      ;; If we get to the root of the org file with no wps property, 
      ;; get the global wps property


      ;;
      ;; TODO: File-level categories are stored not as properties, but
      ;; as keyword elements in the `org-data' structure. In order to
      ;; extract them, it will probaby require a call to
      ;; `org-element-map'. Since this could be an expensive operation
      ;; on an org file with no headline-level categories, but a
      ;; single file-level category, it would need to be cached.
      (unless (equal 'headline (org-element-type current))
        (if (org-global-prop-value "wps")
	    (setq wpsbilling (org-global-prop-value "wps"))
	  (setq wpsbilling "")))) ;;TODO open messasges buffer and alert the user that there is no WPS property
    wpsbilling))


;; Code for getting/setting global properties for a buffer
;; https://emacs.stackexchange.com/a/21472
(require 'cl-lib)

(defun org-global-props-key-re (key)
  "Construct a regular expression matching key and an optional plus and eating the spaces behind.
Test for existence of the plus: (match-beginning 1)"
  (concat "^" (regexp-quote key) "\\(\\+\\)?[[:space:]]+"))

(defun org-global-props (&optional buffer)
  "Get the plists of global org properties of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-equal (org-element-property :key el) "PROPERTY") (nth 1 el))))))

(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer.
Adding up values for one key is supported."
  (let ((key-re (org-global-props-key-re key))
    (props (org-global-props))
    ret)
    (cl-loop with val for prop in props
         when (string-match key-re (setq val (plist-get prop :value))) do
         (setq
          val (substring val (match-end 0))
          ret (if (match-beginning 1)
              (concat ret " " val)
            val)))
    ret))

(defun org-global-prop-set (key value)
  "Set the value of the first occurence of
#+PROPERTY: KEY
add it at the beginning of file if there is none."
  (save-excursion
    (let* ((key-re (org-global-props-key-re key))
       (prop (cl-find-if (lambda (prop)
                   (string-match key-re (plist-get prop :value)))
                 (org-global-props))))
      (if prop
      (progn
        (assert (null (match-beginning 1)) "First occurence of key %s is followed by +." key)
        (goto-char (plist-get prop :begin))
        (kill-region (point) (plist-get prop :end)))
    (goto-char 1))
      (insert "#+PROPERTY: " key " " value "\n"))))
