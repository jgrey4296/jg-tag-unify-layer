(setq-default jg-tag-unify-layer/loc-bookmarks "~/github/writing/resources/main_bookmarks.html"
              jg-tag-unify-layer/loc-bibtex "~/github/writing/resources/years"
              jg-tag-unify-layer/twitter-account-index "~/.spacemacs.d/setup_files/tw_acct.index"
              jg-tag-unify-layer/twitter-tag-index "~/.spacemacs.d/setup_files/tw_tag.index"
              jg-tag-unify-layer/global-tags-location "~/github/writing/resources/collate.tags"

              jg-tag-unify-layer/twitter-helm-candidates nil
              jg-tag-unify-layer/twitter-heading-helm-candidates nil
              jg-tag-unify-layer/preferred-linecount-for-org 1500
              jg-tag-unify-layer/loc-master-tag-list ""
              jg-tag-unify-layer/org-clean-marker nil

              bibtex-completion-bibliography nil
              bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function (lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))

              jg-tag-unify-layer/helm-bibtex-candidates nil

              jg-tag-unify-layer/all-author-list '()
              jg-tag-unify-layer/all-tag-list '()
              jg-tag-unify-layer/global-tags (make-hash-table :test 'equal)
              jg-tag-unify-layer/jg-tag-unify-layer-candidates-names '()
              jg-tag-unify-layer/jg-tag-unify-layer-candidate-counts '()
              ;; Start Position -> End Line number because of changes in positions from tag add/retract
              jg-tag-unify-layer/jg-tag-unify-layer-region '()
              jg-tag-unify-layer/jg-tag-unify-layer-helm `((name . "Helm Tagging")
                                         (action . (("set" . jg-tag-unify-layer/set-tags)))
                                         )
              jg-tag-unify-layer/jg-tag-unify-layer-fallback-source `((name . "")
                                                    (action . (("Create" . jg-tag-unify-layer/set-new-tag)))
                                                    (filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
                                                    )
              )

(add-hook 'bibtex-mode-hook
          (lambda ()
            (let ((misc (assoc "Misc" bibtex-BibTeX-entry-alist))
                  (copied (assoc-delete-all "Misc" (copy-alist bibtex-BibTeX-entry-alist)))
                  (custom '("Misc" "Miscellaneous" nil nil (("author") ("title" "Title of the work (BibTeX converts it to lowercase)") ("howpublished" "The way in which the work was published") ("month") ("year") ("note") ("file") ("keywords")))))
              (setq bibtex-BibTeX-entry-alist (cons custom copied))
              )
            ))
