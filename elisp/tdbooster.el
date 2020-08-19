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

(defvar tdbooster-org-headers "#+LATEX_HEADER: \\usepackage{pgfplots,xcolor,colortbl}
#+PROPERTY: header-args:latex :headers '(\"\\\\usepackage{tikz}\") :fit yes :imagemagick yes :iminoptions -density 300 :imoutoptions
#+OPTIONS: tex:imagemagick
")
(defvar tdbooster-marketinfo-table "
\\begin{center}
\\begin{tabular}{ | c | c | c | }
& 120d & 20d \\\\
\\hline
%s
\\hline
\\end{tabular}
\\end{center}

")

(defvar tdbooster-industry-trend-table "
\\begin{center}
\\begin{tabular}{ | c | %s | }
& %s \\\\
\\hline
%s
\\hline
\\end{tabular}
\\end{center}")


(defvar tdbooster-marketinfo "
{\\huge %s} &
\\begin{tikzpicture}
	\\begin{axis}[hide x axis, hide y axis, height=3cm, width=7cm]
	\\addplot+[mark=., color=black] coordinates { %s };
	\\end{axis}
\\end{tikzpicture} &
\\begin{tikzpicture}
	\\begin{axis}[hide x axis, hide y axis, height=3cm, width=7cm]
	\\addplot+[mark=*, mark size=0.8pt, color=black] coordinates { %s };
	\\end{axis}
\\end{tikzpicture} \\\\
")

(defvar tdbooster--relative-strength "
{\\huge %s} &
\\begin{tikzpicture}
	\\begin{axis}[hide x axis, hide y axis, height=3cm, width=7cm]
	\\addplot+[mark=., color=black] coordinates { %s };
	\\end{axis}
\\end{tikzpicture} \\\\
")
(defvar tdbooster--relative-strength-table "
\\begin{center}
\\begin{tabular}{ | c | c | }
& 120d \\\\
\\hline
%s
\\hline
\\end{tabular}
\\end{center}

")
(defvar tdbooster--table-header
  "| code      | rsi golden cross | rsi6 < 20| rsi6 < 30| ma up| ma arranged|price<ma20|industry| price(-20d)|price(-60d)|price(-120d)|
|-----------+------------------+---+---|")
(defvar tdbooster--table-format "| %s | %s | %s | %s | %s | %s | %s | %S | %s(%f) | %s(%f) | %s(%f) |")
(defvar tdbooster--stat-table-header
  "|code| %s |
|----|")
(defvar tdbooster--stat-table-format "| %s | %s |")



(defun tdbooster--render-one-weekly (json)
  (let* ((code (alist-get 'code json))
	 (weekdata (alist-get 'week_data json))
	 (weeklast (seq-elt (reverse weekdata) 0))
	 (rsi_golden_cross (equal t (alist-get 'rsi_golden_cross weeklast)))
	 (rsi6_lt_20 (equal t (alist-get 'rsi6_lt_20 weeklast)))
	 (rsi6_lt_30 (equal t (alist-get 'rsi6_lt_30 weeklast)))
	 (ma_up (equal t (alist-get 'ma_up weeklast)))
	 (ma_arranged (equal t (alist-get 'ma_arranged weeklast)))
	 (price<ma20 (equal t (alist-get 'price_less_ma20 weeklast)))
	 (price_before_20_price (seq-elt (alist-get 'price_before_20 weeklast) 0))
	 (price_before_20_date (seq-elt (alist-get 'price_before_20 weeklast) 1))
	 (price_before_60_price (seq-elt (alist-get 'price_before_60 weeklast) 0))
	 (price_before_60_date (seq-elt (alist-get 'price_before_60 weeklast) 1))
	 (price_before_120_price (seq-elt (alist-get 'price_before_120 weeklast) 0))
	 (price_before_120_date (seq-elt (alist-get 'price_before_120 weeklast) 1))
	 (industry (alist-get 'industry weeklast)))
    (format tdbooster--table-format code rsi_golden_cross rsi6_lt_20 rsi6_lt_30 ma_up ma_arranged price<ma20 industry
	    price_before_20_date price_before_20_price
	    price_before_60_date price_before_60_price
	    price_before_120_date price_before_120_price)))

(defun tdbooster--render-one-daily (json)
  (let* ((code (alist-get 'code json))
	 (daydata (alist-get 'day_data json))
	 (daylast (seq-elt (reverse daydata) 0))
	 (rsi_golden_cross (equal t (alist-get 'rsi_golden_cross daylast)))
	 (rsi6_lt_20 (equal t (alist-get 'rsi6_lt_20 daylast)))
	 (rsi6_lt_30 (equal t (alist-get 'rsi6_lt_30 daylast)))
	 (ma_up (equal t (alist-get 'ma_up daylast)))
	 (ma_arranged (equal t (alist-get 'ma_arranged daylast)))
	 (price<ma20 (equal t (alist-get 'price_less_ma20 daylast)))
	 (price_before_20_price (seq-elt (alist-get 'price_before_20 daylast) 0))
	 (price_before_20_date (seq-elt (alist-get 'price_before_20 daylast) 1))
	 (price_before_60_price (seq-elt (alist-get 'price_before_60 daylast) 0))
	 (price_before_60_date (seq-elt (alist-get 'price_before_60 daylast) 1))
	 (price_before_120_price (seq-elt (alist-get 'price_before_120 daylast) 0))
	 (price_before_120_date (seq-elt (alist-get 'price_before_120 daylast) 1))
	 (industry (alist-get 'industry daylast)))
    (format tdbooster--table-format code rsi_golden_cross rsi6_lt_20 rsi6_lt_30 ma_up ma_arranged price<ma20 industry
	    price_before_20_date price_before_20_price
	    price_before_60_date price_before_60_price
	    price_before_120_date price_before_120_price)))

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

(defun tdbooster--render-marketinfo-one (data)
  (let* ((title (alist-get 'title data))
	(datalist120d (alist-get 'data data))
	(datalist20d (seq-subseq datalist120d (- (seq-length datalist120d) 20))))
    (format tdbooster-marketinfo title
	    (s-join " " (seq-mapn (lambda (n datapoint)
				    (format "(%d, %f)" n (seq-elt datapoint 1)))
				  (number-sequence 1 200 1) datalist120d))
	    (s-join " " (seq-mapn (lambda (n datapoint)
				    (format "(%d, %f)" n (seq-elt datapoint 1)))
				  (number-sequence 1 200 1) datalist20d)))))

(defun tdbooster--render-marketinfo (json)
  (let ((data (alist-get 'marketinfo json)))
    (format tdbooster-marketinfo-table (s-join "\n\\hline\n" (seq-map #'tdbooster--render-marketinfo-one data)))))

(defun tdbooster--render-relative-strength (json subtable-start subtable-length)
  (let* ((data (alist-get 'data json))
	(r
	 (seq-map (lambda (e)
		    (let* ((code (alist-get 'code e))
			   (daydata (alist-get 'day_data e))
			   (rel (seq-map (lambda (e) (alist-get 'relative_strength e)) daydata)))
		      (format tdbooster--relative-strength code
			      (s-join " " (seq-mapn (lambda (n datapoint)
						      (format "(%d, %f)" n datapoint))
						    (number-sequence 1 200 1) rel)))))
		  (seq-subseq data subtable-start subtable-length))))
    (format tdbooster--relative-strength-table (s-join "\n\\hline\n" r))))

(defun tdbooster--render-industry-trend-title (json)
  (let* ((data (alist-get 'industry_trend json))
	 (title `(,(seq-elt (seq-elt data 0) 0)
		  ,(seq-elt (seq-elt data 1) 0)
		  ,(seq-elt (seq-elt data 2) 0)
		  ,(seq-elt (seq-elt data 3) 0)
		  ,(seq-elt (seq-elt data 4) 0)
		  ,(seq-elt (seq-elt data 5) 0)
		  ,(seq-elt (seq-elt data 6) 0)
		  ,(seq-elt (seq-elt data 7) 0)
		  ,(seq-elt (seq-elt data 8) 0)
		  ,(seq-elt (seq-elt data 9) 0))))
    (s-join "\n" (seq-mapn (lambda (n m)  (format "%d: %s" n m)) (number-sequence 1 (seq-length title) 1) title))))


(defun tdbooster--render-industry-trend (json subtable-start subtable-length)
  (let* ((data (alist-get 'industry_trend json))
	 (title '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "sum"))
	 (datalist (seq-mapn (lambda (e1 e2 e3 e4 e5 e6 e7 e8 e9 e10)
			       (let* ((date1 (seq-elt e1 0))
				      (date2 (seq-elt e2 0))
				      (date3 (seq-elt e3 0))
				      (date4 (seq-elt e4 0))
				      (date5 (seq-elt e5 0))
				      (date6 (seq-elt e6 0))
				      (date7 (seq-elt e7 0))
				      (date8 (seq-elt e8 0))
				      (date9 (seq-elt e9 0))
				      (date10 (seq-elt e10 0))

				      (data1 (* 100 (seq-elt e1 1)))
				      (data2 (* 100 (seq-elt e2 1)))
				      (data3 (* 100 (seq-elt e3 1)))
				      (data4 (* 100 (seq-elt e4 1)))
				      (data5 (* 100 (seq-elt e5 1)))
				      (data6 (* 100 (seq-elt e6 1)))
				      (data7 (* 100 (seq-elt e7 1)))
				      (data8 (* 100 (seq-elt e8 1)))
				      (data9 (* 100 (seq-elt e9 1)))
				      (data10 (* 100 (seq-elt e10 1)))
				      (sum (+ data1 data2 data3 data4
					      data5 data6 data7 data8
					      data9 data10 )))
				 (when (not (and (equal date1 date2)
						 (equal date1 date3)
						 (equal date1 date4)
						 (equal date1 date5)
						 (equal date1 date6)
						 (equal date1 date7)
						 (equal date1 date8)
						 (equal date1 date9)
						 (equal date1 date10))) (error "bad data"))
				 `(,date1 ,(format "\\cellcolor{gray!%.0f}%.0f" data1 data1)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data2 data2)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data3 data3)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data4 data4)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data5 data5)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data6 data6)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data7 data7)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data8 data8)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data9 data9)
					  ,(format "\\cellcolor{gray!%.0f}%.0f" data10 data10)
					  ,(format "\\cellcolor{gray!%.0f}%d" (* 100 (/ sum 1000.0)) sum))))

			     (seq-subseq (seq-elt (seq-elt data 0) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 1) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 2) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 3) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 4) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 5) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 6) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 7) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 8) 1) subtable-start subtable-length)
			     (seq-subseq (seq-elt (seq-elt data 9) 1) subtable-start subtable-length)))
	 (datalist_str (seq-map (lambda (l) (s-concat (s-join " & " l) "\\\\")) datalist)))
    (format tdbooster-industry-trend-table
	    (s-join " | " (make-list (+ 1 (seq-length data)) "c"))
	    (s-join " & " title)
	    (s-join "\n\\hline\n" datalist_str))))

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
    (setq json (json-read-from-string json-string))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert tdbooster-org-headers)
      (insert (tdbooster--render-all-weekly json))
      (org-table-align)
      (insert (tdbooster--render-all-daily json))
      (org-table-align)
      (insert "\n** relative strength\n")
      (insert (tdbooster--render-relative-strength json 0 10))
      (insert (tdbooster--render-relative-strength json 11 20))
      (insert (tdbooster--render-relative-strength json 21 30))
      (insert (tdbooster--render-relative-strength json 31 nil))

      (insert "\n")
      (insert "** marketinfo\n")
      (insert (tdbooster--render-marketinfo json))
      (insert "** industry trend\n")
      (insert (tdbooster--render-industry-trend-title json))
      (insert (tdbooster--render-industry-trend json 0 50))
      (insert (tdbooster--render-industry-trend json 50 100))
      (insert (tdbooster--render-industry-trend json 100 nil))
      (org-latex-preview '(16))
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
      (end-of-line)
      (search-forward "\n\n")
      (condition-case nil
	  (progn
	    (search-forward "(")
	    (let* ((begin (point))
		   (end (progn (forward-list) (point)))
		   (json (json-read-from-string (buffer-substring-no-properties begin end))))
	      (kill-buffer)
	      json))
	(error nil)))))

(defun tdbooster-query (code)
  (interactive (list (ivy-read "> " #'tdbooster--query-function
			       :dynamic-collection t
			       :require-match t
			       :history 'tdbooster-query-history)))
  (message code))


(provide 'tdbooster)
;;; tdbooster.el ends here
