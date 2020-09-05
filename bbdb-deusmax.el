;;; bbdb-deusmax.el --- My bbdb customizations  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/bbdb-deusmax
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (bbdb "3.1") (gnus "5.13"))
;; Keywords: convenience, mail, bbdb, gnus

;; This file is not part of GNU Emacs.

;;; License

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; My bbdb customizations.
;;
;; Currently my customizations affect the automatic collection and updates
;; of bbdb names.
;;
;; The function bbdb-deusmax-add-name used during a bbdb-record update, checks that
;; the new name is not a simple swap of first-name with second-name.
;;
;; Bbdb-mua sets, by default, the customize variable bbdb-mua-auto-update-p to
;; bbdb-select-message. Using customize, my setup sets it to function
;; bbdb-deusmax-select-message. This function makes use of the customize list 
;; bbdb-deusmax-ignore-groups and bbdb-deusmax-ignore-group-level. Using these I can
;; ignore emails for the groups:
;;   1. matching a regex on the list or
;;   2. At a group level and above (the higher, the less important. Default 4)
;;
;; This is gnus specific as it uses gnus specific features.
;;

;;; To use:
;;
;; Set the following customize variables, preferably using the customize interface:
;;
;; (require 'bbdb-deusmax)
;; (setq bbdb-add-name 'bbdb-deusmax-add-name)
;; (setq bbdb-mua-auto-update-p 'bbdb-deusmax-select-message)
;;
;; 

;;; Code:
;;
(require 'gnus)
(require 'bbdb)
(require 'bbdb-mua)

(defgroup bbdb-deusmax nil
  "Bbdb customizations by deusmax."
  :tag "Bbdb Dias"
  :group 'bbdb)

(defcustom bbdb-deusmax-ignore-groups nil
  "A list of regexps matching newsgroups to be ignored.
A list of regexps, to be combined by `gmm-regexp-concate', to match
gnus groups to ignore from updating BBDB records automatically."
  :group 'bbdb-deusmax
  :type '(repeat regexp))

(defcustom bbdb-deusmax-ignore-group-level 4
  "Ignore newsgroups at this level and above.
A integer to ignore groups at this level and above from
automatically updating BBDB records. The default value of 4 will
exclude groups at levels 4, 5 and 6. Used in function
`bbdb-deusmax-select-message'."
  :group 'bbdb-deusmax
  :type 'integer)

(defun bbdb-deusmax-add-name (record newname)
  "Check NEWNAME for adding to existing bbdb RECORD name.
Reject the simple cases, where name elements are in different
order. This seems to be very common with names provided in email.
Otherwise, use the standard bbdb query. This function ignores
case.
It works by spliting the provided names to a list and sorts the
results. Then each list is re-joined to a string. Function
`mapcar' returns the results as a list from which duplicate names
are removed. If the names are the same, only 1 string should be
left."
  (if (eq 1
          (length
           (delete-dups
            (mapcar (lambda (name)
                      (string-join (sort (bbdb-split " " (replace-regexp-in-string "[,.']" "" (downcase name))) #'string<) " "))
                    (list (bbdb-record-name record) newname)))))
      nil
    'query))

(defun bbdb-deusmax-select-message ()
  "Do not add bbdb record for `bbdb-deusmax-ignore-groups'.
Used for `bbdb-mua-auto-update-p'."
  (cond
   ((<= bbdb-deusmax-ignore-group-level (gnus-group-level gnus-newsgroup-name)) nil)
   ((string-match-p (gmm-regexp-concat bbdb-deusmax-ignore-groups) gnus-newsgroup-name) nil)
   (t (bbdb-select-message))))

(provide 'bbdb-deusmax)

;;; bbdb-deusmax ends here.
