;; -*- lexical-binding: t; -*-
;; org-roam-calibre.el --- connect a Calibre library with an org-roam zk.

;; Copyright (C) 2024 Jesse Burke
;; Author: Jesse Burke <jtb445@gmail.com>
;;
;; URL: https://github.com/jesseburke/org-roam-calibre
;; Keywords: org-mode, roam, calibre, 
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org-roam "2.2.2") (calibredb) (cl-lib))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; org-roam-calibre entries are org-roam entries that have a "CALIBREID" property set in
;; their headers. this should be the id of the corresponding entry in the calibre
;; database.

;; this library has commands to choose an org-roam-calibre entry with completing-read, to
;; go from a calibre entry to the corresponding org-roam entry (creating it if necessary),
;; and vice versa.

;; an "entry" is an org-roam-node with a CALIBREID property set.

;; format of "entry" data structure is a list, with entries:
;; ( name(string) file-location(string) properties(alist)
;;
;; an example is:
;; ("Unix programming environment, Kernighan and Pike"
;; "/Dropbox/org-roam/unix_programming_environment_kernighan_and_pike.org"
;; (("CATEGORY" . "unix_programming_environment_kernighan_and_pike") ("CALIBREID" . "2")
;; ("DIR" . "~/Dropbox/org-roam/attachments/unix_programming_environment_kernighan_and_pike")
;; ("ID" . "B39FF72A-F278-4DB2-8B90-1313061B7F92") ("BLOCKED" . "") ("ALLTAGS" .
;; #(":reading:" 1 8 ...)) ("FILE" .
;; "/Dropbox/org-roam/unix_programming_environment_kernighan_and_pike.org")
;; ("PRIORITY" . "B")))

(require 'cl-lib)
(require 'calibredb)

(defgroup org-roam-calibre nil
  "Integration between org-roam and a calibre library."
  :group 'org-roam
  :prefix "org-roam-calibre-"
  :link '(url-link :tag "Github" "https://github.com/jesseburke/org-roam-calibre"))

(defvar orc--calibre-process-buffer-name "*orc-calib-proc-buffer*"
  "The name of the buffer to associate to calibre processes.")

(defvar org-roam-calibre-book-suffix-list '("pdf" "epub" "djvu") "List of suffixes that identify book files.")

;;; functions to get calibreid at point
(defun orc--calibreid-of-entry-at-point ()
  "In org-roam buffer, gets calibreid property value, or nil if doesn't exist."
  (if-let ((return-val (cdr (assoc-string "CALIBREID" (org-roam-node-properties (org-roam-node-at-point))))))
      return-val))

(defun orc--calibreid-at-point ()
  "Function to be passed as interactive code. Works if in an
org-roam-buffer, where entry has a CALIBREID property, or if in
  calibredb-search-mode buffer, then get id from result of
  calibredb-find-candidate-at-point."
  (if (eq major-mode 'calibredb-search-mode)
      (calibredb-getattr (car (calibredb-find-candidate-at-point)) :id)
    (if (eq major-mode 'org-mode)
        (orc--calibreid-of-entry-at-point))))

;;; calibre title related

(defun orc--calibre-title-data-from-id (id)
  "Returns a title, with data type the same as an entry from the returned list of
  =calibredb-candidate= (=calibredb-query-to-alist= applied to result of
  sql query). ID should be a string."
  (if-let* ((valid-id-p (stringp id))
            (query-result (calibredb-query (format "SELECT * FROM (%s) WHERE id = %s" calibredb-query-string id)))
            (valid-query-result-p (or (listp query-result)
                                      (if (stringp query-result)
                                          (not (string= query-result "")))))
            (line (if (listp query-result) (car query-result)
                    (car (split-string (calibredb-chomp query-result) calibredb-sql-newline))))
            (valid-line-p (or (listp line)
                              (string-match-p (concat "^[0-9]\\{1,10\\}" calibredb-sql-separator) line))))
      (list (calibredb-query-to-alist line))))

;; (orc--calibre-title-data-from-id "29")
;; (orc--calibre-title-data-from-id "51")

(defun orc--run-calibredb-cmd-to-string (command-list)
  "Runs a command of calibredb: COMMAND-LIST should be a list of
    strings, that will be passed as arguments to calibredb cli program. E.g.,..."
  (shell-command-to-string (combine-and-quote-strings (cons "calibredb" command-list))))

;; (orc--run-calibredb-cmd-to-string '("show_metadata" "169"))

;;;; author isn't included in title data, only author-sort, is why following
(defun orc--authors-of-title (calibreid)
  "Returns full author(s) string of title with id CALIBREID."
  (let ((metadata-str
         (orc--run-calibredb-cmd-to-string (list "show_metadata" calibreid)))
        (regexp "^Author(s)[ \t]*:[ \t]*\\([^[]+\\)"))
    (if (string-match regexp metadata-str)
        (substring (match-string 1 metadata-str) 0 -1)
      nil)))

;; (orc--authors-of-title "51")

(defun orc--run-calibredb-cmd-async (command-list &optional done-cb-func)
  "Runs a command of calibredb: COMMAND-LIST should be a list of
    strings, that will be passed as arguments to calibredb cli program. E.g.,..."
  (let* ((proc-buf (get-buffer-create orc--calibre-process-buffer-name))
         (old-pt (save-current-buffer
                   (set-buffer proc-buf)
                   (point)))
         (inhibit-message t))
    ;; defining cb instead of passing it as a lambda to stop it being printed in
    ;; messages (for some reason)
    (defun cb-fun (p e)
      (save-current-buffer    
        (set-buffer proc-buf)
        (let ((output-str (buffer-substring-no-properties old-pt (point-max))))
          (cond ((= 1  (process-exit-status p))
                 (message (concat e output-str)))
                ((= 0  (process-exit-status p))
                 (if (and done-cb-func (functionp done-cb-func))
                     (funcall done-cb-func output-str)))))                    
        (goto-char (point-max))))
    (set-process-sentinel
     (apply 'start-process "org-roam-calibre" proc-buf
            "calibredb" command-list)
     #'cb-fun)))

;; (orc--run-calibredb-cmd-async '("show_metadata" "169") (lambda (text) (message text)))

(defun orc--add-files-to-calibre (files &optional done-func delete-originals)
  "FILES is a list of strings, each the pathname to a file to add to
library. If DONE-FUNC is nonnil, will be called with a list of strings, which are the
          calibreids for the newly added entries, in same order as FILES."
  (orc--run-calibredb-cmd-async
   (append '("add") files)
   (lambda (output-text)                             
     (if-let ((id-list (orc--get-book-ids-from-output output-text)))
         (progn
           (if (and done-func (functionp done-func))
               (funcall done-func id-list)
             (message "Added titles, with ids: %s" (prin1-to-string id-list)))
           (calibredb-candidates)
           (calibredb-search-clear-filter)))
     (if delete-originals
         (dolist (file files)
           (message "Deleting %s" file)
           (delete-file file))))))

;; (orc--add-files-to-calibre (list (expand-file-name "~/Desktop/dynkin-markov.pdf")) nil t)
;; (orc--add-files-to-calibre (list (expand-file-name "~/Desktop/dynkin-probs.djvu")))

(defun orc--get-book-ids-from-output (output-str)
  "Looks for strings of the form \"Added book ids:
  ...\" in the string OUTPUT-STR (presumed to be output of a calibre command), and scrapes out the book ids, returning them as a list."
  (let ((regex "Added book ids: \\([0-9, ]+\\)"))
    (if (string-match regex output-str)
        (if-let ((match (match-string 1 output-str)))
            (let ((id-list (split-string match "[, ]+" t)))
              id-list)))))

;; (setq test-output-str "...
;; done
;; Added book ids: 180, 181")
;; (orc--get-book-ids-from-output test-output-str)

(defun org-roam-calibre-add-title-at-point (file)
  "If in a viewer, adds the file being viewed to calibre library."
  (interactive (list (buffer-file-name)))
  (unless (not (orc--is-file-a-book-p file))
    (orc--add-files-to-calibre (list file))))

;;; org-roam entry related

(defun orc--all-entries ()
  "Returns all org-roam entries with a calibreid property. The returned data is: title,
  file, properties."
  (org-roam-db-query
   [:select [title file properties]
	    :from nodes
	    :where (like properties '"%CALIBREID%")]))

;; (orc--all-entries)

(cl-defun orc--choose-entry (&optional (prompt-string "Title: "))
  "To be passed to interactive form, to choose an org-roam-calibre entry."  
  (let* ((orc-entries (orc--all-entries))
         (chosen-title (completing-read "Title: " orc-entries nil 'require-match))
         (entry (assoc chosen-title orc-entries)))
    entry))

;; (setq test-entry (car (orc--all-entries)))

(defun orc--calibreid-of-entry (entry)
       "Returns the value of the calibreid property of ENTRY, and nil if no such
property."
       (cdr (assoc-string "CALIBREID" (nth 2 entry))))

;; expect: "2"
;; (cdr (orc--calibreid-of-entry test-entry))

(defun orc--add-calibreid-prop (calibreid)
  "Adds a \"CALIBREID\" property with value CALIBREID, and \"CALIBRE\" tag, to entry at point."
  (save-excursion
    (goto-char (point-min))
    (org-set-property "CALIBREID" calibreid)
    (org-roam-tag-add '("calibre"))))

(defun orc--get-entry-from-calibreid (calibreid-to-find)
  "Returns entry with calibreid equal to CALIBREID-TO-FIND. If no such entry, returns nil."
  (let ((all-entries (orc--all-entries)))
    (seq-find (lambda (entry) (string= (orc--calibreid-of-entry entry)
                                       calibreid-to-find))
              all-entries)))

;; (orc--get-entry-from-calibreid "2")
;; (orc--get-entry-from-calibreid "51")

(defun org-roam-calibre-find (entry)
  "Completing-read on set of org-roam-calibre entries (entries with a
CALIBREID property)."
  (interactive (list (orc--choose-entry)))
  (let ((file (nth 1 entry)))
    (switch-to-buffer-other-window (find-file-noselect file))))

;;; capture (and related)

(defun orc--file+head-for-entry-from-calibreid (calibreid)
  "Returns a list (or nil) whose car is the file name of the corresponding org-roam
entry, and whose cdr is the first line in the entry. Fetches title
name (from file), book title, author, and published date to make these strings."
  (if-let ((calibre-title-data (orc--calibre-title-data-from-id calibreid))
           (author (orc--authors-of-title calibreid)))
      (let* ((entry-file (concat (file-name-base (calibredb-getattr calibre-title-data :file-path))
                                 " ("
                                 (substring (calibredb-getattr calibre-title-data :book-pubdate) 0 4)
                                 ").org"))
             (proposed-entry-title (concat (calibredb-getattr calibre-title-data :book-title)
                                           " - by "
                                           author
                                           ;; (calibredb-getattr calibre-title-data :author-sort)
                                           " ("
                                           (substring (calibredb-getattr calibre-title-data
                                                                         :book-pubdate) 0 4)
                                           ")"))
             (entry-title (read-string "Entry title: " proposed-entry-title)))
        (cons entry-file
              (concat "#+title: "
                      entry-title
                      "\n\n"
                      (calibredb-getattr calibre-title-data :book-title) ".\n"
                      "By "
                      author
                      ", published in "
                      (substring (calibredb-getattr calibre-title-data :book-pubdate) 0 4) ".")))))

;; (orc--file+head-for-entry-from-calibreid "51")

(defun orc--make-template (file-str head-str)
  `("d" "default" plain
    "%?"
    :if-new (file+head ,file-str ,head-str); ,(concat org-roam-directory "/" file-str) ,head-str)
    :unnarrowed t))

;; (orc--make-template (car (orc--file+head-for-entry-from-calibreid "26")) (cdr (orc--file+head-for-entry-from-calibreid "26")))

(defun org-roam-calibre-capture (calibreid &optional goto)
  "Captures a new org-roam-calibre entry corresponding to calibre title with id CALIBREID, if
  doesn't already exist, with the org-roam-calibre-capture-template being used. If such an
  entry does already exists, does nothing."  
  (if-let ((title-data (orc--file+head-for-entry-from-calibreid calibreid)))
      (let* ((template (orc--make-template (car title-data) (cdr title-data)))
             (node (org-roam-calibre-node-create :title (car title-data) :template
                                                 template)))
        (defun orc--add-cb ()
          (orc--add-calibreid-prop calibreid)
          (remove-hook 'org-roam-capture-new-node-hook #'orc--add-cb))
        (add-hook 'org-roam-capture-new-node-hook #'orc--add-cb)
        (org-roam-capture- :node node
                           :templates (list (org-roam-calibre-node-template node))))))

;; (org-roam-calibre-capture "26")
  
(defun org-roam-calibre-get-create-roam-entry (calibreid)
  "Assumes point is on a calibredb title in calibredb-search-mode, and visits
  org-roam-entry corresponding to the title, creating it via org-capture if necessary."
  (interactive (list (orc--calibreid-at-point)))
  (if-let ((entry (orc--get-entry-from-calibreid calibreid)))
      (org-roam-calibre-find entry)
    (org-roam-calibre-capture calibreid)))

(defun org-roam-calibre-view-description (calibreid)
  "Views the short description of the tile with id CALIBREID"
  (interactive
   (list (orc--calibreid-at-point)))
  (calibredb-show-entry (orc--calibre-title-data-from-id calibreid)))

(defun org-roam-calibre-find-file (calibreid)
  "Calls CALIBREDB-FIND-FILE on the title with id CALIBREID."
  (interactive
   (list (orc--calibreid-at-point)))
  (calibredb-find-file (orc--calibre-title-data-from-id calibreid)))

;;; add attachment from org-roam entry's attachment dir to calibre library

(defun orc--entry-at-point-attachment-dir-and-id ()
  "Returns a pair, with car the path of the attachment directory of the org-roam entry at
point, and cdr the id. Works if point is in the org-roam buffer or in
 Dired, on an org-roam file. Designed to be used in interactive calls."
  (cond ((eq major-mode 'org-mode)
         (list (org-attach-dir nil 'no-fs-check) (org-roam-node-id (org-roam-node-at-point))))
        ((eq major-mode 'dired-mode)
         (let ((file (dired-get-filename nil t)))
           (with-temp-buffer
             (org-mode)
             (insert-file-contents file)
             (list (org-attach-dir nil 'no-fs-check) (org-roam-node-id (org-roam-node-at-point))))))
        (t nil)))

(defun orc--is-file-a-book-p (file)
  (member (file-name-extension file) org-roam-calibre-book-suffix-list))

(defun orc--book-files-in-dir (dir)
  "Returns all book files in DIR, where a file is a book file if its suffix
is in the list ORG-ROAM-CALIBRE-BOOK-SUFFIX-LIST."
  (let ((dir-file-list (directory-files dir))
        (result-list ()))
    (while dir-file-list
      (let ((cur-file (car dir-file-list)))
        (setq dir-file-list (cdr dir-file-list))
        (if (orc--is-file-a-book-p cur-file)
            (setq result-list (cons cur-file result-list)))))
    result-list))

;; (orc--book-files-in-dir "~/Desktop")

(defun orc--choose-book-file-from-dir (dir)
  "If only one book file in DIR, return it. If none, return nil. Else,
 have the user choose among the books."
  (let ((book-list (orc--book-files-in-dir dir)))
    (cond ((= 0 (length book-list)) nil)
          ((= 1 (length book-list)) (concat dir "/" (car book-list)))
          (t (concat dir "/" (completing-read "Choose a book: " book-list nil t))))))

;; (orc--choose-book-file-from-dir "~/Desktop")

(defcustom org-roam-calibre-add-attachment-hook nil
       "Normal hook run in an org-roam entry after a book file is added from its
       attachment directory to the calibre library."
       :type 'hook       
       :group 'org-roam-calibre)

(defun org-roam-calibre-add-attachment-file (attachment-dir roam-id &optional not-delete-originals)
  "Assumes point is in an org entry, and there is a book file in the
  attachments directory of that entry. Prompts user to select one of the
  books. After book is selected, the command adds it to the Calibre
  library, and assigns the new calibreid to the entry as a property."
  (interactive (orc--entry-at-point-attachment-dir-and-id))
  (let ((book-file (orc--choose-book-file-from-dir attachment-dir)))
    (defun callback-for-add (id-list)      
      (if-let ((calibre-id (car id-list)))
          (save-mark-and-excursion
            (set-buffer (find-file-noselect (org-roam-node-file (org-roam-node-from-id roam-id)))) 
            (orc--add-calibreid-prop calibre-id)
            (run-hooks 'org-roam-calibre-add-attachment-hook))))
    (orc--add-files-to-calibre (list (expand-file-name book-file)) 'callback-for-add (not not-delete-originals))))
            
;; (org-roam-calibre-add-attachment-file (expand-file-name "~/Desktop"))

;; org-roam-node-from-id
;; (org-roam-node-id (org-roam-node-at-point))
;; org-roam-node-find
            
