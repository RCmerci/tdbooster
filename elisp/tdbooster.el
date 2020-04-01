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
(require 'ivy)


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
  "| code      | rsi golden cross | rsi6 < 20| rsi6 < 30|
|-----------+------------------+---+---|")
(defvar tdbooster--table-format "| %s | %s | %s | %s |")
(defvar tdbooster--stat-table-header
  "|code| %s |
|----|")
(defvar tdbooster--stat-table-format "| %s | %s |")

(defvar tdbooster-query-history)


(defun tdbooster--render-one-weekly (json)
  (let* ((code (alist-get 'code json))
	 (weekdata (alist-get 'week_data json))
	 (weeklast (seq-elt (reverse weekdata) 0))
	 (rsi_golden_cross (equal t (alist-get 'rsi_golden_cross weeklast)))
	 (rsi6_lt_20 (equal t (alist-get 'rsi6_lt_20 weeklast)))
	 (rsi6_lt_30 (equal t (alist-get 'rsi6_lt_30 weeklast))))
    (format tdbooster--table-format code rsi_golden_cross rsi6_lt_20 rsi6_lt_30)))

(defun tdbooster--render-one-daily (json)
  (let* ((code (alist-get 'code json))
	 (daydata (alist-get 'day_data json))
	 (daylast (seq-elt (reverse daydata) 0))
	 (rsi_golden_cross (equal t (alist-get 'rsi_golden_cross daylast)))
	 (rsi6_lt_20 (equal t (alist-get 'rsi6_lt_20 daylast)))
	 (rsi6_lt_30 (equal t (alist-get 'rsi6_lt_30 daylast))))
    (format tdbooster--table-format code rsi_golden_cross rsi6_lt_20 rsi6_lt_30)))

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


(defun tdbooster--render-stat-one (json)
  (let* ((title (seq-elt json 0))
	 (data (seq-elt json 1))
	 (col-titles (seq-map (lambda (e) (alist-get 'title e)) (alist-get 'column_message (seq-elt data 0)))))
    (s-join "\n"
	    (cons (format "\n*** %s" title)
		  (cons (format tdbooster--stat-table-header (s-join " | " col-titles))
		   (seq-map (lambda (js)
			      (let ((code (alist-get 'code js))
				    (cols (alist-get 'column_message js)))
				(format tdbooster--stat-table-format code
					(s-join " | " (seq-map (lambda (e) (alist-get 'value e)) cols)))))
			    data))))))

(defun tdbooster--render-stat (json)
  (let ((data (alist-get 'data json)))
    (s-join "\n" (seq-map #'tdbooster--render-stat-one data))))

(define-derived-mode tdbooster-mode org-mode "tdbooster"
  "major mode for tdbooster"
  (read-only-mode))

(defun tdbooster-filter (refreshdata)
  "Run tdbooster and show the results in tdbooster-mode."
  (interactive (list (yes-or-no-p "refresh datafiles?")))
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

(defun tdbooster-stat (refreshdata)
  "Run tdbooster statistics and show results in tdbooster-mode"
  (interactive (list (yes-or-no-p "refresh datafiles?")))
  (let* ((args (s-join " " (seq-map (lambda (s) (format "-f %s" s)) tdbooster-datafiles)))
	(buffer (get-buffer-create "*tdbooster-statistics*"))
	(command (format "%s -s oversold %s -o %s %s" tdbooster-bin (if refreshdata "-r" "") tdbooster-datafiles-dir args)))
    (with-current-buffer (get-buffer-create "*tdbooster-stdout*") (erase-buffer))
    (when (not (= 0 (call-process-shell-command command nil "*tdbooster-stdout*")))
      (error (format "something wrong with calling '%s', plz check '*tdbooster-stdout*' buffer" command)))
    (setq json-string (with-current-buffer "*tdbooster-stdout*" (buffer-string)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (tdbooster--render-stat (json-read-from-string json-string)))
      (org-table-align)
      (tdbooster-mode))
    (select-window
     (display-buffer
      buffer
      '((display-buffer-use-some-window))))
    )
  )

(defun tdbooster--to-code (type symbol)
  (concat (cdr (assoc type '(("SH" . "0") ("SZ" . "1") ("HK" . "hk0") ("US" . "US_")))) symbol))

(defun tdbooster--query-function (data)
  (if (s-blank-str? data)
      '()
    (progn
      (setq symbol-info
	    (with-current-buffer (url-retrieve-synchronously
				  (format "http://quotes.money.163.com/stocksearch/json.do?type=&count=10&word=%s" data)
				  t)
	      (beginning-of-buffer)
	      (search-forward "\n\n")
	      (search-forward "(")
	      (let* ((begin (point))
		     (end (progn (forward-list) (point)))
		     (json (seq-map (lambda (e)
				      `(,(alist-get 'type e) ,(alist-get 'symbol e) ,(alist-get 'spell e) ,(alist-get 'name e)))
				    (json-read-from-string (buffer-substring-no-properties begin end)))))
		(kill-buffer)
		json)))
      (setq detail-info (tdbooster--query-detail symbol-info))
      (seq-map (lambda (syminfo) (format "%s.%s %s %s (%+.2f)"
    					 (car syminfo) (cadr syminfo) (caddr syminfo) (cadddr syminfo)
    					 (* 100
					    (alist-get 'percent (alist-get
								 (intern (tdbooster--to-code (car syminfo) (cadr syminfo)))
								 detail-info)
						       0.0))))
	       symbol-info))
    ))

(defun tdbooster--query-detail (type-symbol-list)
  (let ((codes (seq-map (lambda (type-symbol)
			  (tdbooster--to-code (car type-symbol) (cadr type-symbol)))
			type-symbol-list)))
    (with-current-buffer (url-retrieve-synchronously
			  (format "http://api.money.126.net/data/feed/%s" (s-join "," codes)) t)
      (beginning-of-buffer)
      (search-forward "\n\n")
      (search-forward "(")
      (let* ((begin (point))
	     (end (progn (forward-list) (point)))
	     (json (json-read-from-string (buffer-substring-no-properties begin end))))

	json))))

(defun tdbooster-query (code)
  (interactive (list (ivy-read "> " #'tdbooster--query-function
			       :dynamic-collection t
			       :require-match t
			       :history 'tdbooster-query-history)))
  (message code))


(provide 'tdbooster)
;;; tdbooster.el ends here
