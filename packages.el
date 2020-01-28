(defconst jg-tag-unify-layer-packages '(
                               helm
                               helm-bibtex
                               dash
                               f
                               org
                               dired
                               evil
                               (tag-clean-minor-mode :location local)
                               (tag-mode :location local)
                               )
  )

(defun jg-tag-unify-layer/init-f ()
  (use-package f :defer t)
  )
(defun jg-tag-unify-layer/init-dash ()
  (use-package dash :defer t)
  )
(defun jg-tag-unify-layer/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (setq helm-grep-actions (append helm-grep-actions '(("Open Url" . jg-tag-unify-layer/open-url-action))))
    ;; Build a Custom grep for bookmarks
    (setq jg-tag-unify-layer/bookmark-helm-source
          (helm-make-source "Bookmark Helm" 'helm-grep-class
            :action (helm-make-actions "Open Url" 'jg-tag-unify-layer/open-url-action
                                       "Insert"   'jg-tag-unify-layer/insert-candidates
                                       "Insert Link" 'jg-tag-unify-layer/insert-links
                                       "Tweet Link"  'jg-tag-unify-layer/tweet-link-action
                                       )
            :filter-one-by-one 'jg-tag-unify-layer/grep-filter-one-by-one
            :nomark nil
            :backend helm-grep-default-command
            :pcre nil
            )
          jg-tag-unify-layer/twitter-helm-source
          (helm-make-source "Twitter Helm" 'helm-source
            :action (helm-make-actions "File Select Helm" 'jg-tag-unify-layer/file-select-helm)
            )
          jg-tag-unify-lauyer/twitter-heading-helm-source
          (helm-make-source "Twitter Heading Helm" 'helm-source
            :action (helm-make-actions "File Select Helm" 'jg-tag-unify-layer/file-select-helm)
            )
          jg-tag-unify-layer/file-select-source
          (helm-make-source "Twitter File Select Helm" 'helm-source
            :action (helm-make-actions "Find File" 'jg-tag-unify-layer/find-file)
            )
          )
    )
  (spacemacs/declare-prefix "a h" "Helms")
  (spacemacs/set-leader-keys
    "a h f" 'jg-tag-unify-layer/helm-bookmarks
    "a h t" 'jg-tag-unify-layer/helm-twitter
    "a h h" 'jg-tag-unify-layer/helm-heading-twitter
    )

  (defun jg-tag-unify-layer/file-select-helm (candidates)
    " Given a list of Files, provide a helm to open them "
    (interactive)
    ;; (message "File Select Helm Candidates: %s" (helm-marked-candidates))
    ;;process candidates?
    (let*((all-candidates (if (helm-marked-candidates) (-flatten (helm-marked-candidates)) candidates))
          (source (cons `(candidates . ,all-candidates) jg-tag-unify-layer/file-select-source)))
      (helm :sources source
            :full-frame t
            :buffer "*helm file select*"
            )
      )
    )
  (defun jg-tag-unify-layer/helm-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-unify-layer/twitter-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-unify-layer/twitter-helm-candidates '())
          (insert-file jg-tag-unify-layer/twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-unify-layer/twitter-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;add candidates to source
    (let ((source (cons `(candidates . jg-tag-unify-layer/twitter-helm-candidates) jg-tag-unify-layer/twitter-helm-source)))
      ;;call helm
      (helm :sources source
            :full-frame t
            :buffer "*helm twitter*"
            :truncate-lines t
            ;;TODO: is this necessary?
            :candidates jg-tag-unify-layer/twitter-helm-candidates
            )
      )
    )
  (defun jg-tag-unify-layer/helm-heading-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-unify-layer/twitter-heading-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-unify-layer/twitter-heading-helm-candidates '())
          (insert-file jg-tag-unify-layer/twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-unify-layer/twitter-heading-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;add candidates to source
    (let ((source (cons `(candidates . jg-tag-unify-layer/twitter-heading-helm-candidates) jg-tag-unify-layer/twitter-helm-source)))
      ;;call helm
      (helm :sources source
            :full-frame t
            :buffer "*helm twitter heading*"
            :truncate-lines t
            ;;TODO: is this necessary?
            :candidates jg-tag-unify-layer/twitter-heading-helm-candidates
            )
      )
    )
  (defun jg-tag-unify-layer/helm-bookmarks ()
    " Run a Helm for search and opening html bookmarks "
    (interactive)
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-unify-layer/loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-unify-layer/loc-bookmarks)
     'default-directory "~/github/writing/resources/"
     )
    (helm :sources jg-tag-unify-layer/bookmark-helm-source
          :full-frame t
          :buffer "*helm bookmarks*"
          :truncate-lines t
          )
    )

  )
(defun jg-tag-unify-layer/pre-init-helm-bibtex ()
  ;; load the bibliography directory on startup
  (setq bibtex-completion-bibliography (jg-tag-unify-layer/build-bibtex-list))
  ;; Keybind my bib helm
  (spacemacs/set-leader-keys "a h b" 'jg-tag-unify-layer/helm-bibtex)

  ;; Define the bib helm
  (defvar jg-tag-unify-layer/helm-source-bibtex
    '((name . "BibTeX entries")
      (header-name . "BibTeX entries")
      ;; (lambda (name) (format "%s%s: " name (if helm-bibtex-local-bib " (local)" ""))))
      (candidates . helm-bibtex-candidates)
      (match helm-mm-exact-match helm-mm-match helm-fuzzy-match)
      (fuzzy-match)
      (redisplay . identity)
      (multimatch)
      (group . helm)
      (filtered-candidate-transformer helm-bibtex-candidates-formatter helm-flx-fuzzy-matching-sort helm-fuzzy-highlight-matches)
      (action . (("Insert citation"     . helm-bibtex-insert-citation)
                 ("Open PDF"            . helm-bibtex-open-pdf)
                 ("Insert BibTeX key"   . helm-bibtex-insert-key)
                 ("Insert BibTeX entry" . helm-bibtex-insert-bibtex)
                 ("Show entry"          . helm-bibtex-show-entry)
                 )))
    "Simplified source for searching bibtex files")
  (defun jg-tag-unify-layer/helm-bibtex (&optional arg local-bib input)
    " Custom implementation of helm-bibtex"
    (interactive "P")
    (require 'helm-bibtex)
    (when arg
      (bibtex-completion-clear-cache))
    (let* ((bibtex-completion-additional-search-fields '("tags" "year"))
           (candidates (if (or arg (null jg-tag-unify-layer/helm-bibtex-candidates))
                           (progn (message "Generating Candidates")
                                  (bibtex-completion-init)
                                  (setq jg-tag-unify-layer/helm-bibtex-candidates
                                        (mapcar 'jg-tag-unify-layer/process-candidates (bibtex-completion-candidates)))
                                  jg-tag-unify-layer/helm-bibtex-candidates)
                         jg-tag-unify-layer/helm-bibtex-candidates
                         ))
           )
      (helm :sources `(,jg-tag-unify-layer/helm-source-bibtex)
            :full-frame helm-bibtex-full-frame
            :buffer "*helm bibtex*"
            :input input
            :bibtex-local-bib local-bib
            :bibtex-candidates candidates
            )))

  )
(defun jg-tag-unify-layer/post-init-org ()
  (defun jg-tag-unify-layer/org-mod-map ()
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ". c" 'jg-tag-unify-layer/clean-org
      ". w" 'jg-tag-unify-layer/wrap-numbers
      ". L" 'jg-tag-unify-layer/wrap-non-link-urls
      ". D" 'jg-tag-unify-layer/remove-duplicates
      ". s" 'jg-tag-unify-layer/split-on-char-n
      )
    )
  (add-hook 'org-mode-hook 'jg-tag-unify-layer/org-mod-map)

  (jg-tag-unify-layer/rebuild-tag-database)

  (evil-define-operator jg-tag-unify-layer/jg-tag-unify-layer-helm-start (beg end)
    """ Opens the Tagging Helm """
    (interactive "<R>")
    (setq jg-tag-unify-layer/jg-tag-unify-layer-region `(,beg . ,(line-number-at-pos end)))
    (let* ((candidates (jg-tag-unify-layer/jg-tag-unify-layer-candidates))
           (main-source (cons `(candidates . ,(mapcar 'car candidates)) jg-tag-unify-layer/jg-tag-unify-layer-helm))
           )
      (helm :sources '(main-source jg-tag-unify-layer/jg-tag-unify-layer-fallback-source)
            :input "")
      ))

  )
(defun jg-tag-unify-layer/post-init-dired ()
  (spacemacs/declare-prefix-for-mode 'dired-mode
    "m K" "Destructive Edits")
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "K c" 'jg-tag-unify-layer/clean-marked-files
    "K C" 'jg-tag-unify-layer/chop-long-files-from-dired
    "K B" 'jg-tag-unify-layer/unify-pdf-locations
    "K Z" 'jg-tag-unify-layer/quick-compress-orgs
    "K J" 'jg-tag-unify-layer/reformat-jsons

    "t" 'jg-tag-unify-layer/mark-untagged-orgs
    "T" 'jg-tag-unify-layer/dired-directory-count-untagged
    "r" 'jg-tag-unify-layer/find-random-marked-file
    "d" 'jg-tag-unify-layer/describe-marked-tags
    "N" 'jg-tag-unify-layer/display-selection
    )
  )
(defun jg-tag-unify-layer/post-init-evil ()
  (evil-ex-define-cmd "t[ag]" 'jg-tag-unify-layer/jg-tag-unify-layer-helm-start)
  (evil-ex-define-cmd "to" 'jg-tag-unify-layer/tag-occurrences)
  (evil-ex-define-cmd "toa" 'jg-tag-unify-layer/tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"  'org-tags-view)
  (evil-ex-define-cmd "ts"  'org-set-tags)
  )
(defun jg-tag-unify-layer/init-tag-clean-minor-mode ()
  (use-package tag-clean-minor-mode
    :defer t
    :commands (tag-clean-minor-mode)
    :config (progn
              (push 'tag-clean-minor-mode minor-mode-list)
              (spacemacs|define-transient-state tag-clean
                :title "Tag Cleaning Transient State"
                :doc (concat "
                | Commands   ^^|
                |------------^^|------------^^|
                | [_q_] Quit   | [_!_] Split  |
                | [_f_] Filter | [_p_] Prev   |
                | [_s_] Sub    | [_l_] Leave  |
                ")
                :bindings
                ("q" nil :exit t)
                ("f" tag-clean/mark-to-filter)
                ("s" tag-clean/mark-to-sub)
                ("p" tag-clean/previous)
                ("l" tag-clean/leave)
                ("!" jg-tag-unify-layer/org-split-on-headings :exit t)
                )
              (spacemacs/set-leader-keys-for-minor-mode 'tag-clean-minor-mode
                "." 'spacemacs/tag-clean-transient-state/body
                )
              )
    )
  )
(defun jg-tag-unify-layer/init-tag-mode ()
  (use-package tag-mode
    :commands (tag-mode)
    )

  )
