#!/bin/bash
# publish-org-tree.sh
# (c) 2022 Bernd Wachter <bwachter-github@aardsoft.fi>

publish_directory=${publish_directory:-`mktemp -d --suffix=org`}
project_directory=${project_directory:-`pwd`}
project_name=${project_name:-default}
static_files=${static_files:-"css js png jpg gif pdf mp3 ogg zip"}

function publish_project_template(){
cat > "${publish_directory}/project.el" <<-EOF
(require 'org)
(require 'org-table)
(require 'ox-publish)

(load "ox-thtml.el")
(setq backup-inhibited t)
(setq auto-save-default nil)

(setq org-html-validation-link nil)

(setq org-confirm-babel-evaluate nil)
(setq org-export-with-sub-superscripts '{})

(setq publish-directory "${publish_directory}/out")
(setq org-publish-project-alist
      \`(
        ("${project_name}-html"
         :base-directory "${project_directory}"
         :base-extension "org"
         :exclude ,(regexp-opt '("sidebar.org" "README.org"))
         :publishing-directory ,publish-directory
         :html-doctype "html5"
         ;:html-template ,(templated-html-load-template "templates/page.html")
         :recursive t
         :section-numbers nil
         :publishing-function ${org_publishing_function:-org-html-publish-to-html}
         ;:publishing-function org-html-publish-to-templated-html
         :headline-levels 3             ; Just the default for this project.
         :auto-preamble this
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         ;:sitemap-style "tree"
         :with-email nil
         :with-date nil
         :html-head "<link rel=\"stylesheet\" href=\"/css/default.css\" type=\"text/css\"/>"
         )
        ("${project_name}-static"
          :base-directory "${project_directory}"
          :base-extension ,(mapconcat 'symbol-name '(${static_files}) "\\\\|")
          :publishing-directory ,publish-directory
          :recursive t
          :publishing-function org-publish-attachment
          )
        ("${project_name}" :components ("${project_name}-html" "${project_name}-static"))
        ))

(defun publish-project ()
  ""
  (org-publish-project "${project_name}" t))
EOF
}

function publish_project(){
    emacs --batch -Q -L . -eval "(progn (load-file \"${publish_directory}/project.el\")(publish-project))"
}

mkdir -p ${publish_directory}
publish_project_template
publish_project
