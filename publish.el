(require 'package)
(package-initialize)
(unless package-archive-contents
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))
(dolist (pkg '(dash projectile org-plus-contrib yaml-mode htmlize))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'dash)
(require 'org)
(require 'ox-rss)
(require 'ox-publish)
(require 'projectile)

(defun jbrestel--pre/postamble-format (name)
  "Formats the pre/postamble named NAME by reading a file from the snippets directory."
  `(("en" ,(with-temp-buffer
             (insert-file-contents (expand-file-name (format "%s.html" name) "./snippets"))
             (buffer-string)))))

(defun jbrestel--insert-snippet (filename)
  "Format the snippet named FILENAME by reading a file from the snippets directory."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename "./snippets"))
    (buffer-string)))

(defun jbrestel/org-publish-sitemap--valid-entries (entries)
  "Filter ENTRIES that are not valid or skipped by the sitemap entry function."
  (-filter (lambda (x) (car x)) entries))

(defun jbrestel/latest-posts-sitemap-function (title sitemap)
  "posts.org generation. Only publish the latest 5 posts from SITEMAP (https://orgmode.org/manual/Sitemap.html).  Skips TITLE."
  (let* ((posts (cdr sitemap))
         (posts (jbrestel/org-publish-sitemap--valid-entries posts))
         (last-five (seq-subseq posts 0 (min (length posts) 5))))
    (org-list-to-org (cons (car sitemap) last-five))))

(defun jbrestel/archive-sitemap-function (title sitemap)
  "archive.org page (Blog full post list). Wrapper to skip TITLE and just use LIST (https://orgmode.org/manual/Sitemap.html)."
  (let* ((title "Blog") (subtitle "Archive")
         (posts (cdr sitemap))
         (posts (jbrestel/org-publish-sitemap--valid-entries posts)))
    (concat (format "#+TITLE: %s\n\n* %s\n" title subtitle)
            (org-list-to-org (cons (car sitemap) posts))
            "\n#+BEGIN_EXPORT html\n<a href='rss.xml'><i class='fa fa-rss'></i></a>\n#+END_EXPORT\n")))

(defun jbrestel/archive-sitemap-format-entry (entry style project)
  "archive.org and posts.org (latest) entry formatting. Format sitemap ENTRY for PROJECT with the post date before the link, to generate a posts list.  STYLE is not used."
  (let* ((base-directory (plist-get (cdr project) :base-directory))
         (filename (expand-file-name entry (expand-file-name base-directory (jbrestel/project-root))))
         (draft? (jbrestel/post-get-metadata-from-frontmatter filename "DRAFT")))
    (unless (or (equal entry "404.org") draft?)
      (format "%s [[file:%s][%s]]"
              (format-time-string "<%Y-%m-%d>" (org-publish-find-date entry project))
              entry
              (org-publish-find-title entry project)))))

(defun jbrestel/sitemap-for-rss-sitemap-function (title sitemap)
  "Publish rss.org which needs each entry as a headline."
  (let* ((title "Blog") (subtitle "Archive")
         (posts (cdr sitemap))
         (posts (jbrestel/org-publish-sitemap--valid-entries posts)))
    (concat (format "#+TITLE: %s\n\n" title)
            (org-list-to-generic
             posts
             (org-combine-plists
              (list :splice t
                    :istart nil
                    :icount nil
                    :dtstart " " :dtend " "))))))

(defun jbrestel/sitemap-for-rss-sitemap-format-entry (entry style project)
  "Format ENTRY for rss.org for excusive use of exporting to RSS/XML. Each entry needs to be a headline. STYLE is not used."
  (let* ((base-directory (plist-get (cdr project) :base-directory))
         (filename (expand-file-name entry (expand-file-name base-directory (jbrestel/project-root))))
         
         (title (jbrestel/post-get-metadata-from-frontmatter filename "TITLE"))
         ;;(title (org-publish-format-file-entry "%t" filename project))
         ;;(title (org-publish-find-title filename project))
         (date (format-time-string "<%Y-%m-%d>" (org-publish-find-date entry project)))
         (draft? (jbrestel/post-get-metadata-from-frontmatter filename "DRAFT")))
    (unless (or (equal entry "404.org") draft?)
      (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" entry title))
             (org-set-property "RSS_PERMALINK" (concat "posts/" (file-name-sans-extension entry) ".html"))
             (org-set-property "RSS_TITLE" title)
             (org-set-property "PUBDATE" date)
             ;; to avoid second update to rss.org by org-icalendar-create-uid
             ;;(insert-file-contents entry)
             (buffer-string))))
              )

(defun jbrestel/org-html-timestamp (timestamp contents info)
  "We are not going to leak org mode silly <date> format when rendering TIMESTAMP to the world, aren't we?.  CONTENTS and INFO are passed down to org-html-timestamp."
  (let ((org-time-stamp-custom-formats
       '("%d %b %Y" . "%d %b %Y %H:%M"))
        (org-display-custom-times 't))
    (org-html-timestamp timestamp contents info)))

                                        ; We derive our own backend in order to override the timestamp format of the html backend
(org-export-define-derived-backend 'jbrestel/html 'html
  :translate-alist
  '((timestamp . jbrestel/org-html-timestamp)))

(defun jbrestel/post-get-metadata-from-frontmatter (post-filename key)
  "Extract the KEY as`#+KEY:` from POST-FILENAME."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (ignore-errors
        (progn
          (search-forward-regexp (format "^\\#\\+%s\\:\s+\\(.+\\)$" key))
          (match-string 1))))))

(defun jbrestel/org-html-publish-generate-redirect (plist filename pub-dir)
  "Generate redirect files in PUB-DIR from the #+REDIRECT_FROM header in FILENAME, using PLIST."
  (let* ((redirect-from (jbrestel/post-get-metadata-from-frontmatter filename "REDIRECT_FROM"))
         (root (projectile-project-root))
         (pub-root (concat root "public"))
         (new-filepath (file-relative-name filename pub-dir))
         (deprecated-filepath (concat pub-root redirect-from))
         (target-url (concat (file-name-sans-extension new-filepath) ".html"))
         (project (cons 'redirect plist))
         (title (org-publish-find-title filename project)))
    (when redirect-from
      (with-temp-buffer
        (insert (format "This page was moved. [[file:%s][Click here if you are not yet redirected]]." new-filepath))
        (make-directory (file-name-directory deprecated-filepath) :parents)
        (let ((plist (append plist
                             (list :html-head-extra
                                   (format "<meta http-equiv='refresh' content='10; url=%s'>" target-url)))))
          (org-export-to-file 'jbrestel/html deprecated-filepath nil nil nil nil plist))))))

(defun jbrestel/head-common-list (plist)
  "List of elements going in head for all pages.  Takes PLIST as context."
  (let ((description "The blog of John E. Brestelli Jr"))
    (list
;;     (list "link" (list "href" "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css" "rel" "stylesheet" "integrity" "sha256-k2/8zcNbxVIh5mnQ52A0r3a6jAgMGxFJFE2707UxGCk= sha512-ZV9KawG2Legkwp3nAlxLIVFudTauWuBpC10uEafMHYL0Sarrz5A7G79kXh5+5+woxQ5HM559XX2UZjMJ36Wplg==" "crossorigin" "anonymous"))
;;     (list "meta" (list "description" description))
;;     (list "link" (list "rel" "alternate" "type" "application+rss/xml" "title" description "href" "posts/rss.xml")))))
)))
(defun jbrestel/hash-for-filename (filename)
  "Returns the sha25 for FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (secure-hash 'sha256 (current-buffer))))

(defun jbrestel/asset-relative-link-to (resource pub-dir &optional versioned)
    (let* ((assets-project (assoc "assets" org-publish-project-alist 'string-equal))
           (dst-asset (expand-file-name resource (org-publish-property :publishing-directory assets-project)))
           (asset-relative-to-dst-file (file-relative-name dst-asset pub-dir)))
      (if versioned
          (format "%s?v=%s" asset-relative-to-dst-file
                  (jbrestel/hash-for-filename (expand-file-name resource (projectile-project-root))))
        dst-asset asset-relative-to-dst-file)))

(defun jbrestel/org-html-publish-to-html (plist filename pub-dir)
  "Analog to org-html-publish-to-html using jbrestel/html backend.  PLIST, FILENAME and PUB-DIR are passed as is."
  (plist-put plist :html-head
             (concat
;;               (jbrestel--insert-snippet "analytics.js")
               (jbrestel/org-html-head
                (append (jbrestel/head-common-list plist)
                        (plist-get plist :html-head-list)) plist)))
;;  (plist-put plist :html-htmlized-css-url (jbrestel/asset-relative-link-to "css/site.css" pub-dir t))

  (jbrestel/org-html-publish-generate-redirect plist filename pub-dir)
  (org-publish-org-to 'jbrestel/html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))

(defun jbrestel/org-html-head (tags plist)
  "Generate header elements from TAGS.  Accept PLIST for extra context."
  (mapconcat (lambda (x)
               (let ((tag (nth 0 x))
                     (attrs (nth 1 x)))
                 (format "<%s %s/>" tag
                         (mapconcat
                          (lambda (x)
                            (let ((attr (nth 0 x))
                                  (value (nth 1 x)))
                              (when x
                                (format "%s='%s'" attr value)))) (seq-partition attrs 2) " ")))) tags "\n"))

(defun jbrestel/org-html-publish-post-to-html (plist filename pub-dir)
  "Wraps org-html-publish-to-html.  Append post date as subtitle to PLIST.  FILENAME and PUB-DIR are passed."
  (let ((project (cons 'blog plist)))
    (plist-put plist :subtitle
               (format-time-string "%b %d, %Y" (org-publish-find-date filename project)))
    (jbrestel/org-html-publish-to-html plist filename pub-dir)))

(defun jbrestel/project-root ()
  "Thin (zero) wrapper over projectile to find project root."
  (projectile-project-root))

(defun jbrestel/project-relative-filename (filename)
  "Return the relative path of FILENAME to the project root."
  (file-relative-name filename (jbrestel/project-root)))

(defun jbrestel/org-html-publish-site-to-html (plist filename pub-dir)
  "Wraps org-html-publish-to-html.  Append css to hide title to PLIST and other front-page styles.  FILENAME and PUB-DIR are passed."
  (when (equal "index.org" (jbrestel/project-relative-filename filename))
    (plist-put plist :html-head-list
               (list
                (list "link"
                      (list "rel" "stylesheet" "href" (jbrestel/asset-relative-link-to "css/index.css" pub-dir t))))))
  (jbrestel/org-html-publish-to-html plist filename pub-dir))

(defun jbrestel/org-rss-publish-to-rss (plist filename pub-dir)
  "Wrap org-rss-publish-to-rss with PLIST and PUB-DIR, publishing only when FILENAME is 'archive.org'."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

; Project definition
(defvar jbrestel--publish-project-alist
  (list
   ;; generates the main site, and as side-effect, the sitemap for the latest 5 posts
   (list "planning"
         :base-directory "./"
         :include '("project_planning/unidb.org"
		    "project_planning/appDb.org"
		    "project_planning/dataLoad.org"
   		    "project_planning/dataset.org"
		    "project_planning/dnaseq.org"
      		    "project_planning/selenium.org"
		    "project_planning/index.org"
   		    "project_planning/eda.org"
                     "project_planning/eda_megastudy.org"
       		    "project_planning/projectPages.org"
      		    "project_planning/eda_diy.org"
		    "project_planning/apollo.org"
   		    "project_planning/networkAnalysis.org"
      		    "project_planning/geneLists.org"
		    )
         :base-extension "org"
         :publishing-directory (expand-file-name "public" (projectile-project-root))
         :publishing-function 'jbrestel/org-html-publish-site-to-html
	 :section-numbers nil
         :html-preamble nil
         :html-postamble nil
         :html-validation-link nil
         :html-head-include-scripts nil
         :html-head-include-default-style nil)
   (list "SOPs"
         :base-directory "./"
         :include '("SOPs/rnaseq.org"
		    "SOPs/taskBacklog.org"
		    "SOPs/jbrowseFlatFileMigration.org"
       		    "SOPs/taxonomyUpdate.org"
		    )
         :base-extension "org"
         :publishing-directory (expand-file-name "public" (projectile-project-root))
         :publishing-function 'jbrestel/org-html-publish-site-to-html
         :section-numbers nil
         :html-preamble nil
         :html-postamble nil
         :html-validation-link nil
         :html-head-include-scripts nil
         :html-head-include-default-style nil)
   (list "assets"
         :base-directory "./"
         :exclude (regexp-opt '("assets" "public"))
	          :include '("LICENSE" ".nojekyll" "publish.el")
         :recursive t
         :base-extension (regexp-opt '("jpg" "gif" "png" "js" "svg" "css"))
         :publishing-directory "./public"
         :publishing-function 'org-publish-attachment)))

                                        ; Our publishing definition
(defun jbrestel-publish-all ()
  "Publish the blog to HTML."
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (perl . t)
     (dot . t)
     (R . t)
     (sql . nil)
     ))
  (let ((make-backup-files nil)
        (org-publish-project-alist       jbrestel--publish-project-alist)
        ;; deactivate cache as it does not take the publish.el file into account
        (user-full-name "John E. Brestelli Jr.")
        (user-mail-address "jbrestell@gmail.com")
        (org-src-fontify-natively t)
        (org-publish-cache nil)
        (org-publish-use-timestamps-flag nil)
        (org-export-with-section-numbers nil)
        (org-export-with-smart-quotes    t)
;;        (org-export-with-toc             nil)
        (org-export-with-sub-superscripts '{})
        (org-html-divs '((preamble  "header" "preamble")
                         (content   "main"   "content")
                         (postamble "footer" "postamble")))
        (org-html-container-element         "section")
        (org-html-metadata-timestamp-format "%d %b. %Y")
        (org-html-checkbox-type             'html)
        (org-html-html5-fancy               t)
        (org-html-validation-link           nil)
        (org-html-doctype                   "html5")
        (org-html-htmlize-output-type       'css)
        (org-plantuml-jar-path (-first 'file-exists-p
                                       ; openSUSE, Ubuntu
                                       '("/usr/share/java/plantuml.jar" "/usr/share/plantuml/plantuml.jar")))
        (org-confirm-babel-evaluate
         (lambda (lang body)
           (message (format "in lambda %s" lang))
           (not (member lang '("dot" "plantuml" "emacs-lisp" "sql"))))))
    (org-publish-all)))

(provide 'publish)
;;; publish.el ends here

(setq org-link-abbrev-alist
      '(("redmine"        . "https://redmine.apidb.org/issues/")
	("readtheorgs"    . "https://jbrestel.github.io/org-docs")
        ("orgdocs"        . "https://github.com/jbrestel/org-docs/tree/main")
	))


(setq org-todo-keywords
        '((sequence "TODO" "THEM" "ASSIGN" "IN PROGRESS" "IN_PROGRESS" "QA" "BLOCKED" "|" "DONE")))


  (setq org-todo-keyword-faces
        `(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("DONT" . "#f2241f")
     ("BLOCKED" . "#f2241f")
     ("ASSIGN" . "#FF00FF")
;;     ("DONE" . "#86dc2f")
     ("IN PROGRESS" . "#b1951d")
     ("IN_PROGRESS" . "#b1951d")
          ("QA" . "#ADD8E6")
     ("\\?\\?\\?+" . "#dc752f")))

)
