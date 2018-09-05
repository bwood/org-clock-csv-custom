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
		   (org-clock-csv--escape (plist-get plist ':category)))
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
	   (duration (org-element-property :duration element)))
      
      (list :task task
            :parents parents
            :category category
            :start start
            :end end
	    :duration duration
            :effort effort
            :ishabit ishabit
            :tags tags)))))
