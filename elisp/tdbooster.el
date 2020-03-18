;;; tdbooster.el --- major mode for tdbooster        -*- lexical-binding: t; -*-

;; Copyright (C) 2019 rcmerci

;; Author: rcmerci <rcmerci@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'seq)
(require 'json)
(require 's)



(defcustom tdbooster-bin "tdbooster"
  "command to call tdbooster binary"
  :type 'string
  :group 'tdbooster)


(defcustom tdbooster-datafiles-dir nil
  "datafiles dir "
  :type 'string
  :group 'tdbooster)

(defcustom tdbooster-datafiles nil
  "datafile list for analysis "
  :type '(repeat string)
  :group 'tdbooster)


(defvar tdbooster--table-header
  "| code      | rsi golden cross | rsi6 < 40| rsi6 < 30|
|-----------+------------------+---+---|")
(defvar tdbooster--table-format "| %s | %s | %s | %s |")

(defun tdbooster--render-one-weekly (json)
  (let* ((code (alist-get 'code json))
	 (weekdata (alist-get 'week_data json))
	 (weeklast (seq-elt (reverse weekdata) 0))
	 (rsi_golden_cross (equal t (alist-get 'rsi_golden_cross weeklast)))
	 (rsi6_lt_40 (equal t (alist-get 'rsi6_lt_40 weeklast)))
	 (rsi6_lt_30 (equal t (alist-get 'rsi6_lt_30 weeklast))))
    (format tdbooster--table-format code rsi_golden_cross rsi6_lt_40 rsi6_lt_30)))

(defun tdbooster--render-one-daily (json)
  (let* ((code (alist-get 'code json))
	 (daydata (alist-get 'day_data json))
	 (daylast (seq-elt (reverse daydata) 0))
	 (rsi_golden_cross (equal t (alist-get 'rsi_golden_cross daylast)))
	 (rsi6_lt_40 (equal t (alist-get 'rsi6_lt_40 daylast)))
	 (rsi6_lt_30 (equal t (alist-get 'rsi6_lt_30 daylast))))
    (format tdbooster--table-format code rsi_golden_cross rsi6_lt_40 rsi6_lt_30)))

(defun tdbooster--render-all-weekly (json)
  (s-join "\n" (cons "\n** WEEK"
		    (cons  tdbooster--table-header
			   (seq-map (lambda (e)
				      (tdbooster--render-one-weekly e))
				    (alist-get 'data json))))))
(defun tdbooster--render-all-daily (json)
  (s-join "\n" (cons "\n** DAY"
		     (cons  tdbooster--table-header
			    (seq-map (lambda (e)
				       (tdbooster--render-one-daily e))
				     (alist-get 'data json))))))

(define-derived-mode tdbooster-mode org-mode "tdbooster"
  "major mode for tdbooster"
  (read-only-mode))

(defun tdbooster (refreshdata)
  "Run tdbooster and show the results in tdbooster-mode."
  (interactive (list (yes-or-no-p "refresh datafile?")))
  (let* ((args (s-join " " (seq-map (lambda (s) (format "-f %s" s)) tdbooster-datafiles)))
	 (buffer (get-buffer-create "*tdbooster*"))
	 (command (format "%s %s -o %s %s"  tdbooster-bin (if refreshdata "-r" "") tdbooster-datafiles-dir args)))
    (with-current-buffer (get-buffer-create "*tdbooster-stdout*") (erase-buffer))
    (when (not (= 0 (call-process-shell-command command nil "*tdbooster-stdout*")))
      (error (format "something wrong with calling '%s', plz check '*tdbooster-stdout*' buffer" command)))
    (setq json-string (with-current-buffer "*tdbooster-stdout*" (buffer-string)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (tdbooster--render-all-weekly (json-read-from-string json-string)))
      (org-table-align)
      (insert (tdbooster--render-all-daily (json-read-from-string json-string)))
      (org-table-align)
      (tdbooster-mode))
    (select-window
     (display-buffer
      buffer
      '((display-buffer-use-some-window))))))


(provide 'tdbooster)
;;; tdbooster.el ends here
