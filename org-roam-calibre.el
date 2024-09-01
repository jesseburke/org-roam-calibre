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

;;; calibre title related

(defun orc--calibreid-of-entry-at-point ()
  (if-let ((return-val (cdr (assoc-string "CALIBREID" (org-roam-node-properties (org-roam-node-at-point))))))
          return-val))

(defun orc--calibreid-at-point ()
  "Function to be passed to interactive. Works if in an org-roam-buffer,
  where entry has a CALIBREID property, or if in calibredb-search-mode
  buffer, then get id from result of calibredb-find-candidate-at-point."
  (if (eq major-mode 'calibredb-search-mode)
      (calibredb-getattr (car (calibredb-find-candidate-at-point)) :id))
  (if (eq major-mode 'org-mode)
      (orc--calibreid-of-entry-at-point)))

(defun orc--calibre-title-from-id (id)
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

;; (orc--calibre-title-from-id "29")

;;; org-roam entry related

(defun orc--all-entries ()
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

(defun org-roam-calibre-find (entry)
  "Completing-read on set of org-roam-calibre entries (entries with a CALIBREID property)."
  (interactive (list (orc--choose-entry)))
  (let ((file (nth 1 entry)))
    (switch-to-buffer-other-window (find-file-noselect file))))

;; (setq test-entry (car (orc--all-entries)))

(defun orc--calibreid-of-entry (entry)
       "Returns the value of the calibreid property of ENTRY, and nil if no
      such property."
       (cdr (assoc-string "CALIBREID" (nth 2 entry))))

;; expect: "2"
;; (cdr (orc--calibreid-of-entry test-entry))

(defun orc--get-entry-from-calibreid (calibreid-to-find)
  "Returns entry with calibreid equal to CALIBREID-TO-FIND. If no such entry, returns nil."
  (let ((all-entries (orc--all-entries)))
    (seq-find (lambda (entry) (string= (orc--calibreid-of-entry entry)
                                 calibreid-to-find))
              all-entries)))

;; (orc--get-entry-from-calibreid "2")

;;; capture (and related)

(cl-defstruct (org-roam-calibre-node (:include org-roam-node) (:constructor org-roam-calibre-node-create))
  "Add a template slot to org-roam-node struct."
  template)

;; (org-roam-calibre-node-create :template "test")

(defun orc--file+head-for-entry-from-calibreid (calibreid)
  "Returns a list (or nil) whose car is the file name of the corresponding org-roam
entry, and whose cdr is the first line in the entry. Fetches title
name (from file), book title, author, and published date to make these strings."
  (let* ((calibre-title-data (orc--calibre-title-from-id calibreid))
         (entry-file (concat (file-name-base (calibredb-getattr calibre-title-data :file-path))
                              " ("
                              (substring (calibredb-getattr calibre-title-data :book-pubdate) 0 4)
                              ").org"))
         (proposed-entry-title (concat (calibredb-getattr calibre-title-data :book-title)
                                       " - "
                                       (calibredb-getattr calibre-title-data :author-sort)
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
                  ;; TODO: get author instead of author-sort
                  (calibredb-getattr calibre-title-data :author-sort) 
                  ", published in "
                  (substring (calibredb-getattr calibre-title-data :book-pubdate) 0 4) "."))))

;; (orc--file+head-for-entry-from-calibreid "29")

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
        (defun orc--add-calibreid-prop ()
          (save-excursion
            (goto-char (point-min))
            (org-set-property "CALIBREID" calibreid)
            (org-roam-tag-add '("calibre")))
          (remove-hook 'org-roam-capture-new-node-hook #'orc--add-calibreid-prop))
        (add-hook 'org-roam-capture-new-node-hook #'orc--add-calibreid-prop)
        (org-roam-capture- :node node
                           :templates (list (org-roam-calibre-node-template node))))))

;; (org-roam-calibre-capture "26")
  
(defun org-roam-calibre-get-create-roam-entry (calibreid)
  "Assumes point is on a calibredb title in calibredb-search-mode, and visits
  org-roam-entry corresponding to the title, creating it via org-capture if necessary."
  (interactive (list (orc--calibreid-at-point)))
  (if-let ((entry (orc--entry-from-calibreid calibreid)))
      (org-roam-calibre-find entry)
    (org-roam-calibre-capture calibreid)))

(defun org-roam-calibre-view-description (calibreid)
      (interactive
       (list (orc--calibreid-at-point)))
      (calibredb-show-entry (orc--calibre-title-from-id calibreid)))

(defun org-roam-calibre-find-file (calibreid)
  (interactive
   (list (orc--calibreid-at-point)))
  (calibredb-find-file (orc--calibre-title-from-id calibreid)))
