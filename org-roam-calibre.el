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

(cl-defun orc--choose-entry (&optional (prompt-string "Title: "))
  "To be passed to interactive form, to choose an org-roam-calibre entry."  
  (let* ((orc-entries (org-roam-db-query
                       [:select [title file properties]
	                        :from nodes
	                        :where (like properties '"%CALIBREID%")]))
         (chosen-title (completing-read "Title: " orc-entries nil 'require-match))
         (entry (assoc chosen-title orc-entries)))
    entry))

(defun org-roam-calibre-find (entry)
  (interactive (list (orc--choose-entry)))
  (let ((file (nth 1 entry)))
    (switch-to-buffer-other-window (find-file-noselect file))))
  



;; (message (completing-read "Choose Calibre entry: " (calibredb-candidates)))
