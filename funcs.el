;; bibtex
(defun jg-tag-unify-layer/build-bibtex-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (setq bibtex-completion-bibliography (directory-files jg-tag-unify-layer/loc-bibtex 't "\.bib$")))

(defun jg-tag-unify-layer/bibtex-set-tags (x)
  " Set tags in bibtex entries "
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg-tag-unify-layer/jg-tag-unify-layer-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-pos jg-tag-unify-layer/jg-tag-unify-layer-marker)
         (current-tags '())
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 jg-tag-unify-layer/global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate jg-tag-unify-layer/global-tags) 1) jg-tag-unify-layer/global-tags))
                       )))
         )
    (save-excursion
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (progn (setq current-tags (split-string (bibtex-autokey-get-field "tags") "," t " +")
                     prior-point (point))
               (mapc add-func actual-candidates)
               (bibtex-set-field "tags" (string-join current-tags ","))
               (org-ref-bibtex-next-entry)
               )))
    )
  )
(defun jg-tag-unify-layer/bibtex-set-new-tag (x)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (let ((prior-point (- (point) 1))
          (end-pos jg-tag-unify-layer/jg-tag-unify-layer-marker)
          (stripped_tag (jg-tag-unify-layer/strip_spaces x))
          )
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (setq prior-point (point))
        (let* ((current-tags (split-string (bibtex-autokey-get-field "tags") "," t " +")))
          (if (not (-contains? current-tags stripped_tag))
              (progn
                (push stripped_tag current-tags)
                (puthash stripped_tag 1 jg-tag-unify-layer/global-tags)))
          (bibtex-set-field "tags" (string-join current-tags ","))
          (org-ref-bibtex-next-entry)
          ))))
  )
(defun jg-tag-unify-layer/unify-pdf-locations-in-file (name)
  "Change all pdf locations in bibtex file to relative,
ensuring they work across machines "
  (message "Unifying Locations in %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (while (re-search-forward "file[[:digit:]]* ?= *{\\(.+mega\\)/\\(mendeley\\)?" nil t)
      (replace-match "~/Mega" nil nil nil 1)
      (if (eq 6 (length (match-data)))
          (replace-match "pdflibrary" t nil nil 2))
      )
    (write-file name)
    )
  )
(defun jg-tag-unify-layer/unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/unify-pdf-locations-in-file files)
    )
  )

;; file processing
(defun jg-tag-unify-layer/chop-long-file (name &optional preferred-length)
  "Take long org files and split them into multiple files
If preferred-length is not specified, use jg-tag-unify-layer/preferred-linecount-for-org
"
  (message "----------")
  (message "Chopping: %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (let* ((count 1)
           (base_name (file-name-sans-extension name))
           (internal_name (buffer-substring (+ 2 (point-min)) (line-end-position)))
           (master_name (format "%s_master.org" base_name))
           (regexp "^\\*\\*[^*]")
           (last-position (re-search-forward regexp nil t))
           (linecount 0)
           (fn-fn (lambda () (format "%s_%s.org" base_name count)))
           (ln-fn (lambda (a b) (- (line-number-at-pos (max a b))
                                   (line-number-at-pos (min a b)))))
           )
      (append-to-file (format "* %s\n" internal_name) nil master_name)
      (while (re-search-forward "^\\*\\*[^*]" nil t )
        (if (not (file-exists-p (funcall fn-fn)))
            (progn (message "Creating %s" (funcall fn-fn))
                   (append-to-file (format "* %s %s\n" internal_name count) nil (funcall fn-fn))
                   (append-to-file (format "** [[%s][%s %s]]\n" (funcall fn-fn) internal_name count) nil master_name)
                   )
          )
        (append-to-file "\n** " nil (funcall fn-fn))
        (append-to-file last-position (line-beginning-position) (funcall fn-fn))
        (setq linecount (+ linecount (funcall ln-fn (point) last-position))
              last-position (point))
        (if (> linecount (or preferred-length jg-tag-unify-layer/preferred-linecount-for-org))
            (setq linecount 0
                  count (+ 1 count))
          )
        )
      (append-to-file "\n** " nil (funcall fn-fn))
      (append-to-file last-position (point-max) (funcall fn-fn))
      )
    )
  )
(defun jg-tag-unify-layer/chop-long-files-from-dired ()
  "Subdivide marked files if they are too long"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/chop-long-file files)
    )
  )
(defun jg-tag-unify-layer/move-links ()
  " Go through all links in a file,
and either copy, or move, the the referenced file to a new location
Prefix-arg to move the file otherwise copy it
"
  (interactive)
  ;;Specify target, or use default
  (let ((target (read-directory-name "Move To: "
                                     "/Volumes/Overflow/missing_images/"))
        (action (if current-prefix-arg 'rename-file 'copy-file))
        link
        )
    (if (not (file-exists-p target))
        (progn (message "Making target directory: %s" target)
               (mkdir target))
      )
    (message "Process Type: %s" action)
    (goto-char (point-min))
    (while (eq t (org-next-link))
      (setq link (plist-get (plist-get (org-element-context) 'link) :path))
      (message "Processing: %s" link)
      (if (not (f-exists? (f-join target (-last-item (f-split link)))))
          (funcall action link target)
        (message "Already Exists"))
      )
    )
  )

;; org cleaning
(defun jg-tag-unify-layer/fill-paragraph-step-back()
  (interactive)
  (goto-char (point-max))
  (let ((prog (lambda ()
                (let ((elem (cadr (org-element-at-point))))
                  (fill-region-as-paragraph
                   (getf elem :begin) (getf elem :end)))))
        curr-elem
        return-to
        )
    (while (not (eq (point) (point-min)))
      (setq curr-elem (org-element-at-point)
            return-to (getf (cadr curr-elem) :begin))
      (cond ((eq (car curr-elem) 'property-drawer)
             (goto-char (- (getf (cadr curr-elem) :begin) 1))
             )
            ((eq (car curr-elem) 'headline)
             (goto-char (- (getf (cadr curr-elem) :begin) 1))
             )
            (t
             (goto-char (getf (cadr curr-elem) :begin))
             (funcall prog)
             (insert "\n")
             )
            )
      (goto-char (- return-to 1))
      )
    )
  )
(defun jg-tag-unify-layer/dired-clean-orgs (name)
  "A Wrapper around clean org to back up the original file "
  (message "----------")
  (message "Cleaning: %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (write-file (format "%s_orig" name))
    (org-mode)
    (jg-tag-unify-layer/clean-org t)
    (write-file name)
    )
  (message "Finished Cleaning")
  )
(defun jg-tag-unify-layer/clean-marked-files ()
  "Clean marked Org files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/dired-clean-orgs files)
    )
  )
(defun jg-tag-unify-layer/clean-org (&optional skipfck)
  "The Main Clean-org routine"
  (interactive)
  (message "Starting Org Clean")
  (message "Hiding Properties")
  ;; indent region
  ;;wrap lines that are too long
  (goto-char (point-min))
  (jg-tag-unify-layer/wrap-non-link-urls)
  (spacemacs/indent-region-or-buffer)
  (jg-tag-unify-layer/fill-paragraph-step-back)
  (whitespace-cleanup)
  (org-show-all)
  ;;Find all pic.twitter's and ensure on new line
  (goto-char (point-min))
  (message "Finding pic.twitter's")
  (while (search-forward "pic.twitter" nil t)
    (let ((sub (buffer-substring (line-beginning-position) (point))))
      (if (not (eq 0 (string-match "^[[:space:]]+pic.twitter" sub)))
          (progn
            (backward-char (+ 1 (length "pic.twitter")))
            (insert "\n\n")))
      (progn (while (eq 0 (string-match "^[[:space:]]*$" (buffer-substring (line-beginning-position -0)
                                                                           (line-end-position -0))))
               (join-line)
               )
             (goto-char (line-beginning-position))
             (insert "\n")
             (forward-line))
      )
    )
  ;; Clean Whitespace
  (message "Cleaning Whitespace")
  (setq jg-tag-unify-layer/org-clean-marker (make-marker))
  (org-map-entries 'jg-tag-unify-layer/map-entries-clean-whitespace t nil)
  (set-marker jg-tag-unify-layer/org-clean-marker nil)

  ;; Tidy all links:
  ;; DO NOT USE ORG-NEXT-LINK
  ;; it ignores links in property drawers
  (goto-char (point-min))
  (while (re-search-forward org-link-any-re nil t)
    (set-marker jg-tag-unify-layer/org-clean-marker (point))
    (goto-char (car (match-data 0)))
    (let ((prev-line (buffer-substring (line-beginning-position 0)
                                       (line-end-position 0))))
      (cond  ((eq 0 (string-match "^[[:space:]]+:PERMALINK:" prev-line))
              (join-line))
             ((eq 0 (string-match "^[[:space:]]+:PROPERTIES:" prev-line))
              nil)
             ((not (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                         (line-beginning-position)
                                                         (point)))))
              (insert "\n")
              )
             (t
              (while (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                           (line-beginning-position 0)
                                                           (line-end-position 0))))
                (join-line)
                )
              )
             )
      )
    (goto-char jg-tag-unify-layer/org-clean-marker)
    )

  (message "Indenting")
  (spacemacs/indent-region-or-buffer)
  (whitespace-cleanup)
  (setq jg-tag-unify-layer/org-clean-marker nil)

  ;;Find and replace
  (goto-char (point-min))
  (while (re-search-forward "]\\[\n[[:space:]]+" nil t)
    (replace-match "][")
    )

  (if (not skipfck)
      (progn
        (goto-char (point-min))
        (while (re-search-forward  "file:.+?%.+$" nil t)
          (if (jg-spacemacs-org/link-not-exists-p)
              (progn
                (goto-char (line-beginning-position))
                (insert "--->")
                (if (s-equals? (read-string "Delete line? ") "y")
                    (delete-region (line-beginning-position) (line-end-position))
                  (progn
                    (delete-region (line-beginning-position)
                                   (+ (line-beginning-position) (length "--->")))
                    (forward-line))
                  ))
            )
          )
        )
    )

  (org-cycle-hide-drawers 'all)
  (goto-char (point-min))
  (message "Org Clean Finished")
  )
(defun jg-tag-unify-layer/map-entries-clean-whitespace ()
  "Called from org-map-entries. reduces whitespace prior
to point to a single new line"
  (set-marker jg-tag-unify-layer/org-clean-marker (line-end-position))
  (if (not (eq (point) (point-min)))
      (progn
        (while (eq 0 (string-match "^[[:space:]]*$"
                                   (buffer-substring
                                    (line-beginning-position 0)
                                    (line-end-position 0))))
          (join-line))
        (if (not (string-equal "*" (buffer-substring
                                    (line-beginning-position 0)
                                    (+ 1 (line-beginning-position 0)))))
            (insert "\n"))
        (setq org-map-continue-from jg-tag-unify-layer/org-clean-marker)
        )
    )
  )
(defun jg-tag-unify-layer/wrap-numbers (a b)
  "Find numbers and wrap them in parentheses to avoid org treating them as lists"
  (interactive "r")
  (message "%s %s" a b )
  (goto-char a)
  (while (re-search-forward "^[[:space:]]*\\([[:digit:]]+\\)[.)] " b t)
    (replace-match "\n(\\1)")
    )
  )
(defun jg-tag-unify-layer/wrap-non-link-urls ()
  "Find urls that are not wrapped into org link format, and wrap them"
  (interactive)
  (let ((start (if (eq evil-state 'visual) evil-visual-beginning (point-min)))
        (last (if (eq evil-state 'visual) evil-visual-end  nil)))
    (goto-char start)
    (while (re-search-forward "[^[[]\\(http[^ …]+\\)" last t)
      (replace-match "[[\\1][ᵢ]]")
      )
    )
  )
(defun jg-tag-unify-layer/build-permalink-regions()
  " To be run with org-map-entries, extracts permalinks and regions ready to remove duplicates"
  (let* ((entry-details (cadr (org-element-at-point)))
         (permalink (plist-get entry-details :PERMALINK))
         (begin (plist-get entry-details :begin))
         (level (plist-get entry-details :level))
         )
    (if (and permalink begin level (string-match "\\[\\[.+?\\]\\[\\(.+?\\)\\]\\]" permalink))
        `(,(match-string 1 permalink) ,begin ,level)
      nil)
    )
  )
(defun jg-tag-unify-layer/remove-duplicates ()
  "Find duplicate tweets in an org file and remove them"
  (interactive)
  (let ((permalinks (make-hash-table :test 'equal))
        ;;Get all entries' (permalink start-bound end-bound level)
        (all-entries (seq-filter 'identity (org-map-entries 'jg-tag-unify-layer/build-permalink-regions)))
        (to-remove '())
        (archive-buffer (get-buffer-create "*Org-Cleanup-Tweets*"))
        )
    ;;Process all-entries, storing the first instance of a tweet in the hashmap,
    ;;all subsequent instances in the to-remove list
    (cl-loop for tweet in all-entries
             do (if (not (gethash (nth 0 tweet) permalinks))
                    (let ((m (make-marker)))
                      (move-marker m (nth 1 tweet))
                      (puthash (nth 0 tweet) m  permalinks))
                  (push tweet to-remove)
                  )
             )
    (message "To Remove: %s" (length to-remove))
    ;;Now remove those duplicates
    (cl-loop for tweet in to-remove
             do (let* ((begin (nth 1 tweet))
                       (end (progn
                              (goto-char begin)
                              (plist-get (cadr (org-element-at-point)) :end))))
                  (message "Getting: %s" tweet)
                  ;; copy tweet into deletion buffer
                  (princ (buffer-substring-no-properties begin (- end 1))
                         archive-buffer)
                  ;; replace it in the original with a replaced link to the remaining
                  (delete-region begin (- end 1))
                  (goto-char begin)
                  (insert (format "%s Duplicate of %s\n\n"
                                  (make-string (nth 2 tweet) ?*)
                                  (nth 0 tweet)))
                  )
             )
    )
  )

(defun jg-tag-unify-layer/org-format-temp-buffer (name source_name)
  " Format bar chart buffer as an org buffer.
Adds a header, separates similar counted lines into sub headings,
and sorts groups alphabetically"
  (with-current-buffer name
    (org-mode)
    (let ((inhibit-read-only 't)
          (last_num nil)
          (get_num_re ": \\([[:digit:]]+\\) +:")
          (start-marker (make-marker))
          (end-marker (make-marker))
          (sort-fold-case t)
          matched
          )
      ;;Loop over all lines
      (goto-char (point-min))
      (set-marker start-marker (point))
      (while (re-search-forward get_num_re nil 't)
        (setq matched (match-string 1))
        (cond
         ((not last_num) t)
         ((not (string-equal last_num matched))
          (progn (set-marker end-marker (line-beginning-position))
                 (if (> (- end-marker 1) start-marker)
                     (sort-lines nil start-marker (- end-marker 1)))
                 (goto-char start-marker)
                 (insert "** ")
                 (goto-char end-marker)
                 (set-marker start-marker end-marker)
                 )))
        (setq last_num matched)
        (forward-line)
        )
      ;;clean up last group:
      (set-marker end-marker (line-beginning-position))
      (if (> end-marker start-marker)
          (sort-lines nil start-marker (- end-marker 1)))
      (goto-char start-marker)
      (insert "** ")
      ;;Add Header:
      (goto-char (point-min))
      (insert "* Tag Summary for: " source_name "\n")
      (indent-region (point-min) (point-max))
      )
    )
  )
(defun jg-tag-unify-layer/org-split-temp-buffer-create (args)
  "Given a pair, create a temp buffer in the cdr directory,
naming the directory based on the first line of text and insert the car "
  ;; (message "Creating Temp buffer for: %s" args)
  (assert (f-dir? (cdr args)))
  (with-temp-buffer
    (org-mode)
    (insert (car args))
    (goto-char (point-min))
    (re-search-forward "^\\*\\* ")
    (write-file (f-join (cdr args) (format "%s.org" (string-trim (buffer-substring (point) (line-end-position))))))
    )
  )
(defun jg-tag-unify-layer/org-split-on-headings ()
  " Split an org file into multiple smaller buffers non-destructively "
  (interactive)
  (let ((contents (buffer-substring (point-min) (point-max)))
        (target-depth (read-number "What Depth Subtrees to Copy? "))
        (target-dir (read-directory-name "Split into directory: "))
        (map-fn (lambda ()
                  (let* ((components (org-heading-components))
                         (depth (car components)))
                    ;;Only copy correct depths
                    (if (eq depth target-depth)
                        (progn
                          ;; (message (format "Current : %s %s" count (nth 4 components)))
                          (org-copy-subtree 1)
                          (current-kill 0 t)
                          )
                      )
                    )
                  ))
        results
        )
    (with-temp-buffer
      (org-mode)
      (insert contents)
      (goto-char (point-min))
      (setq results (-non-nil (org-map-entries map-fn)))
      )
    (-each (-zip-fill target-dir results '()) 'jg-tag-unify-layer/org-split-temp-buffer-create)
    )
  )
(defun jg-tag-unify-layer/org-split-alphabetically ()
  " Split a buffer of values on separate lines into headings alphabetically "
  (interactive)
  (goto-char (point-min))
  (let ((current ?a)
        (end ?z))
    (insert "* Top\n")
    (while (and (<= current end)
                (re-search-forward (format "^%c" current)))
      (goto-char (line-beginning-position))
      (insert (format "** %c\n" current))
      (incf current)
      )
    )
  )
(defun jg-tag-unify-layer/org-split-tag-list ()
  " Combine the org-split functions into a single routine.
Sort, align, split, save "
  (interactive)
  (let ((text (buffer-string))
        (sort-fold-case t))
    (with-temp-buffer
      (insert text)
      (sort-lines nil (point-min) (point-max))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\):" 1 nil t)
      (jg-tag-unify-layer/org-split-alphabetically)
      (jg-tag-unify-layer/org-split-on-headings)
      )
    )
  )

;; helm actions
(defun jg-tag-unify-layer/open-url-action (x)
  " An action added to helm-grep for loading urls found in bookmarks "
  (let* ((marked (helm-marked-candidates))
         (no-props (mapcar (lambda (x) (substring-no-properties x 0 (length x))) marked))
         link-start-point
         )
    (with-temp-buffer
      (mapc (lambda (x) (insert (format "%s\n" x))) no-props)
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        ;; (search-forward "href=\"")
        ;; (setq link-start-point (point))
        ;; (search-forward "\" TAGS")
        ;; (backward-char (length "\" TAGS"))
        (message "Opening: %s" (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-beginning-position))
        (jg-spacemacs-org/open_link_externally)
        (forward-line)
        )
      )
    )
  )
(defun jg-tag-unify-layer/tweet-link-action (candidate)
  "Helm action to open a tweet buffer with the link inserted"
  (evil-window-new (get-buffer-window helm-current-buffer)
                   "*Link Tweeting*")
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (evil-window-set-height 10)
  (evil-initialize-local-keymaps)
  (evil-local-set-key 'normal
                      (kbd "C-c C-C") 'jg-tag-unify-layer/tweet-link-finish)
  (insert "\n")
  (insert candidate)
  (redraw-display)
  )
(defun jg-tag-unify-layer/tweet-link-finish ()
  "Action to finish and tweet a link"
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max))))
    (jg-spacemacs-twitter/twitter-tweet-text text nil '(jg-spacemacs-twitter/tweet_sentinel))
    ))
(defun jg-tag-unify-layer/org-set-tags (x)
  """ Improved action to add and remove tags Toggle Selected Tags
Can operate on regions of headings """
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg-tag-unify-layer/jg-tag-unify-layer-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-pos jg-tag-unify-layer/jg-tag-unify-layer-marker)
         (current-tags '())
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 jg-tag-unify-layer/global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate jg-tag-unify-layer/global-tags) 1) jg-tag-unify-layer/global-tags))
                       ))))
    (save-excursion
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (progn (setq current-tags (org-get-tags nil t)
                     prior-point (point))
               (mapc add-func actual-candidates)
               (org-set-tags current-tags)
               (org-forward-heading-same-level 1)
               )))))
(defun jg-tag-unify-layer/insert-candidates (x)
  "A Helm action to insert selected candidates into the current buffer "
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n")))))
(defun jg-tag-unify-layer/insert-links (x)
  "Helm action to insert selected candidates formatted as org links"
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (format "[[%s][%s]]" (substring x 0 -2) (substring x 0 -2))) candidates "\n")))))
(defun jg-tag-unify-layer/grep-filter-one-by-one (candidate)
  "A Grep modification for bookmark helm to extract a bookmark's url and tags"
  (if (consp candidate)
      ;; Already computed do nothing (default as input).
      candidate
    (let* ((line   (helm--ansi-color-apply candidate))
           (split  (helm-grep-split-line line))
           ;; Normalize Size of this:
           (lineno (nth 1 split))
           (norm_ln (s-append (s-repeat (- 6 (string-width lineno)) " ") lineno))
           ;; The Actual Line:
           (str    (nth 2 split))
           (sub    (substring str (or (s-index-of "HREF=" str) 0)))
           (tag_index (s-index-of "TAGS=" sub))
           (url (substring sub (string-width "HREF=\"") tag_index))
           (tags (substring sub (+ (string-width "HREF=\"") (or tag_index 0)) (s-index-of ">" sub)))
           (chopped_tags (substring tags 0 (min 50 (string-width tags))))
           (norm_tags (s-append (s-repeat (- 50 (string-width chopped_tags)) " ") chopped_tags))
           )
      (cons (concat (propertize norm_ln 'face 'helm-grep-lineno)
                    (propertize (concat ": " norm_tags) 'face 'rainbow-delimiters-depth-3-face)
                    (propertize (concat ": " url) 'face 'rainbow-delimiters-depth-1-face))
            (or url line))
      )
    )
  )
(defun jg-tag-unify-layer/find-file (x)
  "A simple helm action to open selected files"
  (let ((files (if (helm-marked-candidates) (helm-marked-candidates) (list x))))
    (mapc 'find-file (mapcar 'string-trim files))
    )
  )
(defun jg-tag-unify-layer/process-candidates (x)
  "Utility to tidy bibtex-completion-candidates for helm-bibtex"
  (cons (s-replace-regexp ",? +" " " (car x))
        (cdr x))
  )

;; tags
(defun jg-tag-unify-layer/get-buffer-tags (&optional name depth)
  "Process a buffer and get all tags from a specified depth of heading
if no depth is specified, get all tags of all headings
returns a hash-table of the tags, and their instance counts.
"
  (let ((tag-set (make-hash-table :test 'equal))
        (tagdepth-p (if (and depth (> depth 0)) (lambda (x) (eq depth x)) 'identity))
        )
    ;; (message "Get buffer tags: %s %s" name tagdepth-p)
    (with-current-buffer (if name name (current-buffer))
      (save-excursion ;;store where you are in the current
        (goto-char (point-min))
        ;;where to store tags:
        ;;split tags into list
        (mapc (lambda (x) (incf (gethash x tag-set 0)))
              ;;TODO: fix tag depth filtering
              (-flatten
               (org-map-entries (lambda () (if (funcall tagdepth-p (car (org-heading-components)))
                                               (org-get-tags nil t) '())))))
        tag-set
        )
      )
    )
  )
(defun jg-tag-unify-layer/get-file-tags (filename &optional depth)
  "Get tags from a specified file, at an org specified depth.
If depth is not specified, default to get all tags from all headings
Return a hash-table of tags with their instance counts"
  (let ((tagcounts (make-hash-table :test 'equal))
        (tagdepth-p (if (and depth (> depth 0)) (lambda (x) (eq depth x)) 'identity))
        raw-tags
        )
    ;; (message "Get file tags: %s %s" filename depth)
    (with-temp-buffer
      (insert-file filename)
      (org-mode)
      (goto-char (point-min))
      (setq raw-tags (org-map-entries (lambda () (if (funcall tagdepth-p (car (org-heading-components))) (org-get-tags nil t) '()))))
      )
    (mapc (lambda (x) (incf (gethash x tagcounts 0))) (-flatten raw-tags))
    tagcounts
    )
  )

(defun jg-tag-unify-layer/tag-occurrences ()
  " Create a Bar Chart of Tags in the current buffer "
  (interactive)
  (let* ((depth-arg evil-ex-argument)
         (depth (if depth-arg (string-to-number depth-arg) nil))
         (alltags (make-hash-table :test 'equal))
         )
    (if (eq 'org-mode major-mode)
        (progn
          ;; (message "Getting Tags for all buffers to depth: %s" depth)
          (maphash (lambda (k v) (incf (gethash k alltags 0) v)) (jg-tag-unify-layer/get-buffer-tags nil depth))
          (if (not (hash-table-empty-p alltags))
              (jg-tag-unify-layer/chart-tag-counts alltags (buffer-name))
            (message "No Tags in buffer")))
      (message "Not in an org buffer")
      )
    )
  )
(defun jg-tag-unify-layer/tag-occurrences-in-open-buffers()
  """ Retrieve all tags in all open buffers, print to a temporary buffer """
  (interactive "p")
  (let* ((allbuffers (buffer-list))
         (alltags (make-hash-table :test 'equal))
         (depth (if depth-arg (string-to-number depth-arg) nil))
         )
    ;; (message "Getting Tags for all buffers to depth: %s" depth)
    (loop for x in allbuffers do
          (if (with-current-buffer x (eq 'org-mode major-mode))
              (maphash (lambda (k v) (if (not (gethash k alltags)) (puthash k 0 alltags))
                         (incf (gethash k alltags) v)) (jg-tag-unify-layer/get-buffer-tags x depth))
            )
          )
    (if (not (hash-table-empty-p alltags))
        (jg-tag-unify-layer/chart-tag-counts alltags "Active Files")
      (message "No Tags in buffers"))
    )
  )

(defun jg-tag-unify-layer/org-tagged-p  (filename)
  "Test an org file. Returns true if the file has tags for all depth 2 headings"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (org-mode)
    (let* ((mapped (org-map-entries (lambda () `(,(car (org-heading-components)) ,(org-get-tags nil t)))))
           (filtered (seq-filter (lambda (x) (and (eq 2 (car x)) (null (cadr x)))) mapped)))
      (seq-empty-p filtered)
      )
    )
  )

(defun jg-tag-unify-layer/set-tags (x)
  "Utility action to set tags. Works in org *and* bibtex files"
  (if (eq major-mode 'bibtex-mode)
      (jg-tag-unify-layer/bibtex-set-tags x)
    (jg-tag-unify-layer/org-set-tags x))
  )
(defun jg-tag-unify-layer/set-new-tag (x)
  "Utility action to add a new tag. Works for org *and* bibtex"
  (if (eq major-mode 'bibtex-mode)
      (jg-tag-unify-layer/bibtex-set-new-tag x)
    (jg-tag-unify-layer/org-set-new-tag x))
  )
(defun jg-tag-unify-layer/org-set-new-tag (x)
  "Utility to set a new tag for an org heading"
  (save-excursion
    (let ((prior-point (- (point) 1))
          (end-pos jg-tag-unify-layer/jg-tag-unify-layer-marker)
          (stripped_tag (jg-tag-unify-layer/strip_spaces x))
          )
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (setq prior-point (point))
        (let* ((current-tags (org-get-tags nil t)))
          (if (not (-contains? current-tags stripped_tag))
              (progn
                (push stripped_tag current-tags)
                (puthash stripped_tag 1 jg-tag-unify-layer/global-tags)))
          (org-set-tags current-tags)
          (org-forward-heading-same-level 1)
          )))))

(defun jg-tag-unify-layer/chart-tag-counts (counthash name)
  "Given a hashtable of counts, create a buffer with a bar chart of the counts"
  ;; (message "Charting: %s %s" counthash name)
  (let* ((hashPairs (-zip (hash-table-keys counthash) (hash-table-values counthash)))
         (sorted (sort hashPairs (lambda (a b) (> (cdr a) (cdr b)))))
         (maxTagLength (apply 'max (mapcar (lambda (x) (length (car x))) sorted)))
         (maxTagAmnt (apply 'max (mapcar (lambda (x) (cdr x)) sorted)))
         )
    ;;print them all out

    (with-temp-buffer-window "*Tags*"
                             nil
                             nil
                             ;; Todo: Expand this func to group and add org headings
                             (mapc (lambda (x) (princ (format "%s\n" x)))
                                   (jg-tag-unify-layer/make-bar-chart sorted maxTagLength maxTagAmnt))
                             )
    (jg-tag-unify-layer/org-format-temp-buffer "*Tags*" name)
    )
  )
(defun jg-tag-unify-layer/make-bar-chart (data maxTagLength maxTagAmnt)
  " Make a bar chart from passed in hashtable and descriptive information "
  (let* ((maxTagStrLen (length (number-to-string maxTagAmnt)))
         (maxTagLength-bounded (min 40 maxTagLength))
         (max-column (- fill-column (+ 3 maxTagLength-bounded maxTagStrLen 3 3)))
         (bar-div (/ (float max-column) maxTagAmnt)))
    (mapcar 'jg-tag-unify-layer/bar-chart-line data)))
(defun jg-tag-unify-layer/bar-chart-line (x)
  "Construct a single line of a bar chart"
  (let* ((tag (car x))
         (tag-len (length tag))
         (tag-cut-len (- maxTagLength-bounded 3))
         (tag-truncated-p (> tag-len maxTagLength-bounded))
         (tag-substr (if tag-truncated-p (string-join `(,(substring tag nil tag-cut-len) "..."))
                       tag))
         (tag-final-len (length tag-substr))
         (amount (cdr x))
         (amount-str (number-to-string amount))
         (sep-offset (- (+ 3 maxTagLength-bounded) tag-final-len))
         (amount-offset (- maxTagStrLen (length amount-str)))
         (bar-len (ceiling (* bar-div amount)))
         )
    (string-join `(,tag-substr
                   ,(make-string sep-offset ?\ )
                   " : "
                   ,amount-str
                   ,(make-string amount-offset ?\ )
                   " : "
                   ,(make-string bar-len ?=)
                   ;; "\n"
                   )))
  )
(defun jg-tag-unify-layer/barchart-region()
  " Create a Bar Chart of value pairs of the selected region "
  (interactive)
  (assert (eq evil-state 'visual))
  (let* (;;grab the region
         (text (buffer-substring-no-properties evil-visual-beginning
                                               evil-visual-end))
         ;;split text into pairs
         (lines (split-string text "\n" t " +"))
         (pairs (mapcar (lambda (x) (split-string x ":" t " +")) lines))
         (count-hash (make-hash-table :test 'equal))
         )
    ;; (message "Getting Tags for all buffers to depth: %s" depth)
    (mapcar (lambda (x) (incf (gethash (car x) count-hash 0) (string-to-number (cadr x)))) pairs)
    (if (not (hash-table-empty-p count-hash))
        (jg-tag-unify-layer/chart-tag-counts count-hash (buffer-name))
      (message "No Tags in buffer")))
  )

(defun jg-tag-unify-layer/select-random-tags (n)
  (interactive "nHow many tags? ")
  (let* ((tags (hash-table-keys jg-tag-unify-layer/global-tags))
         (selection (mapcar (lambda (x) (seq-random-elt tags)) (make-list n ?a)))
         )
    (with-temp-buffer-window "*Rand Tags*"
                             'display-buffer-pop-up-frame
                             nil
                             (mapc (lambda (x) (princ x ) (princ "\n")) selection)
                             )
    )
  )

;; dired actions
(defun jg-tag-unify-layer/describe-marked-tags ()
  "Describe tags in marked files"
  (interactive)
  (let ((marked (dired-get-marked-files))
        (targetdepth (or current-prefix-arg 2))
        (alltags (make-hash-table :test 'equal))
        )
    ;; (message "Describing marked file tags to depth: %s" targetdepth)
    (loop for x in marked do
          (maphash (lambda (k v) (incf (gethash k alltags 0) v)) (jg-tag-unify-layer/get-file-tags x targetdepth))
          )
    (if (not (hash-table-empty-p alltags))
        (jg-tag-unify-layer/chart-tag-counts alltags "Dired Marked Files")
      (message "No Tags in Files")
      )
    )
  )
(defun jg-tag-unify-layer/mark-untagged-orgs ()
  "Mark org files which are not tagged at heading depth 2"
  (interactive)
  (dired-map-over-marks
   (progn (if (or (not (f-ext? (dired-get-filename) "org"))
                  (jg-tag-unify-layer/org-tagged-p (dired-get-filename)))
              (dired-unmark 1)))
   nil
   )
  )
(defun jg-tag-unify-layer/dired-directory-count-untagged ()
  "Count marked org files that are untagged"
  (interactive)
  (let ((counts 0)
        (untagged-p (lambda (x) (not (jg-tag-unify-layer/org-tagged-p x))))
        )
    (dired-map-over-marks
     (if (f-dir? (dired-get-filename))
         (incf counts (length
                       (seq-filter untagged-p (directory-files-recursively (dired-get-filename) "\.org"))
                       )
               )
       )
     nil
     )
    (message "%s Org files untagged" counts)
    )
  )
(defun jg-tag-unify-layer/find-random-marked-file ()
  "Open random file from marked"
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (find-file (nth (random (length marked))
                    marked))
    )
  )
(defun jg-tag-unify-layer/quick-compress-orgs ()
  "Find all orgs in cwd down, compress together"
  (interactive)
  (let* ((curr default-directory)
         (files (directory-files-recursively curr "\\.org"))
         (target_dir "compressed_orgs")
         )
    ;;Make the top level
    (if (not (f-exists? (f-join curr target_dir)))
        (mkdir (f-join curr target_dir)))
    ;;Copy files over
    (mapc (lambda (x)
            (let ((target (f-join curr target_dir (-last-item (f-split (f-parent x))))))
              (if (not (f-exists? target)) (mkdir target))
              (copy-file x (format "%s/" target))
              )
            ) files)
    (dired-compress-file (f-join curr target_dir))
    (delete-directory (f-join curr target_dir) t t)
    )
  )
(defun jg-tag-unify-layer/open-selection (pair)
  "Open only a selection of a large file "
  (let ((file (car pair))
        (selection-size (cdr pair))
        selection)
    (with-temp-buffer
      (insert-file file)
      (goto-char (random (- (point-max) selection-size)))
      (setq selection (buffer-substring (point) (+ (point) selection-size)))
      )
    (with-temp-buffer-window (format "*%s - selection*" (-last-item (f-split file)))
                             nil nil
                             (princ selection)
                             )
    )
  )
(defun jg-tag-unify-layer/display-selection (n)
  "Open only a selection of large files "
  (interactive "nNum Chars: ")
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/open-selection
              (-zip-fill n files '()))
    )
  )

;; utility
(defun jg-tag-unify-layer/strip_spaces (str)
  "Utility to replace spaces with underscores in a string.
Used to guard inputs in tag strings"
  (s-replace " " "_" (string-trim str))
  )
(defun jg-tag-unify-layer/sort-candidates (ap bp)
  " Sort routine to sort by colour then lexicographically "
  (let* ((a (car ap))
         (b (car bp))
         (aprop (get-text-property 0 'font-lock-face a))
         (bprop (get-text-property 0 'font-lock-face b))
         (lookup (lambda (x) (gethash (cadr x) jg-tag-unify-layer/global-tags))))
    (cond
     ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
     ((and aprop (not bprop)) t)
     ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
     )))
(defun jg-tag-unify-layer/jg-tag-unify-layer-candidates ()
  " Given Candidates, colour them if they are assigned, then sort them  "
  (let* ((buffer-cand-tags (jg-tag-unify-layer/get-buffer-tags))
         (global-tags jg-tag-unify-layer/global-tags))
    (if (not (hash-table-empty-p global-tags))
        (let* ((cand-keys (hash-table-keys global-tags))
               (cand-vals (hash-table-values global-tags))
               (cand-pairs (-zip cand-keys cand-vals))
               (maxTagLength (apply 'max (mapcar 'length cand-keys)))
               (maxTagAmount (apply 'max cand-vals))
               (bar-keys (jg-tag-unify-layer/make-bar-chart cand-pairs maxTagLength maxTagAmount))
               (display-pairs (-zip bar-keys cand-keys))
               (current-tags (org-get-tags nil t))
               (propertied-tags (map 'list (lambda (candidate)
                                             (let ((candString (car candidate)))
                                               (if (-contains? current-tags (cdr candidate))
                                                   (progn (put-text-property 0 (length candString)
                                                                             'font-lock-face
                                                                             'rainbow-delimiters-depth-1-face
                                                                             candString)))
                                               `(,candString ,(cdr candidate)))) display-pairs))
               )
          (setq jg-tag-unify-layer/jg-tag-unify-layer-candidate-counts global-tags)
          (setq jg-tag-unify-layer/jg-tag-unify-layer-candidates-names (sort propertied-tags 'jg-tag-unify-layer/sort-candidates))
          )
      '()
      ))
  )
(defun jg-tag-unify-layer/split-on-char-n (beg end n)
  "Loop through buffer, inserting newlines between lines where
the nth char changes"
  (interactive "r\nnSplit on char number: ")
  (save-excursion
    (goto-char beg)
    (let ((last-char (downcase (char-after (+ (point) n))))
          curr-char)
      (while (< (point) end)
        (setq curr-char (downcase (char-after (+ (point) n))))
        (if (not (eq last-char curr-char))
            (progn
              (setq last-char curr-char)
              (goto-char (line-beginning-position))
              (insert "\n")
              )
          )
        (forward-line)
        )
      )
    )
  )

(defun jg-tag-unify-layer/next-similar-string ()
  " Go through lines, finding the next adjacent string pair
uses org-babel-edit-distance "
  (interactive)
  (let* ((bound (or current-prefix-arg jg-tag-unify-layer/last-similarity-arg))
         (curr-sim (+ bound 1))
         (s2 (downcase (string-trim (car (s-split ":" (buffer-substring (line-beginning-position) (line-end-position)))))))
         s1
         )
    (while (and (< (point) (point-max))
                (> curr-sim bound))
      (forward-line)
      (setq jg-tag-unify-layer/last-similarity-arg bound)
      (setq s1 s2)
      (setq s2 (downcase (string-trim (car (s-split ":" (buffer-substring (line-beginning-position) (line-end-position)))))))
      (setq curr-sim (org-babel-edit-distance s1 s2))
      )
    )
  (message "Using distance %s" jg-tag-unify-layer/last-similarity-arg)
  )
;; Indexing
(defun jg-tag-unify-layer/rebuild-tag-database ()
  "Rebuild the tag database from global-tags-location "
  (interactive)
  (clrhash jg-tag-unify-layer/global-tags)
  (if (f-exists? jg-tag-unify-layer/global-tags-location)
      (with-temp-buffer
        (insert-file jg-tag-unify-layer/global-tags-location)
        (goto-char (point-min))
        (while (< (point) (point-max))
          ((lambda (x) (puthash (car x) (string-to-number (cadr x)) jg-tag-unify-layer/global-tags)) (split-string (buffer-substring (line-beginning-position) (line-end-position)) ":" nil " +"))
          (forward-line)
          )
        )
    (message "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY")
    )
  )
(defun jg-tag-unify-layer/index-people ()
  "Index all twitter users in the current directory "
  (interactive)
  ;; Get all org files
  (let ((all-orgs (directory-files-recursively (dired-current-directory) "\.org$"))
        (index-hash (make-hash-table :test 'equal))
        (inserted-for-file (make-hash-table :test 'equal))
        (curr-d (dired-current-directory))
        )
    (message "Found %s org files" (length all-orgs))
    ;; For each org collect people in file
    (cl-loop for filename in all-orgs
             do (with-temp-buffer
                  (clrhash inserted-for-file)
                  (insert-file filename)
                  (goto-char (point-min))
                  (while (re-search-forward "\*+ \\(@[_[:word:]]+\\)" nil t)
                    (let ((str (match-string 1)))
                      (if (null (gethash str inserted-for-file))
                          (progn (puthash str t inserted-for-file)
                                 (if (null (gethash str index-hash))
                                     (puthash str '() index-hash))
                                 ;; (message "Pushing %s : %s" str filename)
                                 (push filename (gethash str index-hash))))))))
    ;; create index
    (message "Accumulated %s accounts" (length (hash-table-keys index-hash)))
    (with-temp-buffer
      (maphash (lambda (k v)
                 (insert (format "%s "k))
                 (mapc (lambda (x)
                         (insert (format ":%s" x))) v)
                 (insert "\n")
                 ) index-hash)
      (write-file jg-tag-unify-layer/twitter-account-index t)
      )
    )
  (message "Finished writing file")
  )
(defun jg-tag-unify-layer/index-tags()
  " Run routine to index all tags in org files "
  (interactive)
  ;; Get all org files
  (let ((all-orgs (directory-files-recursively (dired-current-directory) "\.org$"))
        (index-hash (make-hash-table :test 'equal))
        (inserted-for-file (make-hash-table :test 'equal))
        (curr-d (dired-current-directory))
        )
    (message "Found %s org files" (length all-orgs))
    ;; For each org collect tags in file
    (cl-loop for filename in all-orgs
             do (with-temp-buffer
                  (org-mode)
                  (clrhash inserted-for-file)
                  (insert-file filename)
                  (goto-char (point-min))
                  ;;Get tags:
                  (while (re-search-forward "^\\*\\* " nil t)
                    (let ((tags (org-get-tags nil t)))
                      (mapc (lambda (x)
                              (if (null (gethash x inserted-for-file))
                                  (progn (puthash x t inserted-for-file)
                                         (if (null (gethash x index-hash))
                                             (puthash x '() index-hash))
                                         (push filename (gethash x index-hash))
                                         )
                                )
                              )
                            tags)
                      )
                    )
                  )
             )
    ;; create index
    (message "Accumulated %s tags" (length (hash-table-keys index-hash)))
    (with-temp-buffer
      (maphash (lambda (k v)
                 (insert (format "%s "k))
                 (mapc (lambda (x)
                         (insert (format ":%s" x))) v)
                 (insert "\n")
                 ) index-hash)
      (write-file jg-tag-unify-layer/twitter-tag-index t)
      )
    )
  (message "Finished writing file")
  )

;;json
(defun jg-tag-unify-layer/reformat-json-file (file)
  (assert (f-ext? file "json"))
  (with-temp-buffer
    (insert-file file)
    (json-mode-beautify)
    (write-file (format "%s_cleaned.json" (f-join (f-parent file)
                                                  (f-base file))))
    )
  )
(defun jg-tag-unify-layer/reformat-jsons ()
  "Beautify marked json files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/reformat-json-file files)
    )
  )
