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



(defmacro tdbooster--name (json) `(alist-get 'name ,json))
(defmacro tdbooster--translist (json) `(alist-get 'trans ,json))
(defmacro tdbooster--trans-I (json) `(alist-get 'I ,json))
(defmacro tdbooster--trans-O (json) `(alist-get 'O ,json))
(defmacro tdbooster--trans-logs (json) `(alist-get 'logs ,json))
(defmacro tdbooster--trans-warnings (json) `(alist-get 'warnings ,json))
(defmacro tdbooster--trans-I-date (json) `(alist-get 'date ,json))
(defmacro tdbooster--trans-I-price (json) `(alist-get 'price ,json))
(defmacro tdbooster--trans-I-info (json) `(alist-get 'info ,json))
(defmacro tdbooster--trans-O-date (json) `(alist-get 'date ,json))
(defmacro tdbooster--trans-O-price (json) `(alist-get 'price ,json))
(defmacro tdbooster--trans-O-info (json) `(alist-get 'info ,json))
(defmacro tdbooster--attentionlist (json) `(alist-get 'attentions ,json))
(defmacro tdbooster--attention-logs (json) `(alist-get 'logs ,json))
(defmacro tdbooster--attention-warnings (json) `(alist-get 'warnings ,json))


(tdbooster--trans-O-date nil)

(defconst tdbooster--template-header
  "
* ${name} ${I-price-latest}/${O-price-latest} ${focus}
** 当前关注
    :PROPERTIES:
    :VISIBILITY: folded
    :END:
${attentions}
** 交易
${transactions}
")

(defconst tdbooster--template-transaction
  "*** ${I-date}/${I-price} - ${O-date}/${O-price} ${diff}
    :PROPERTIES:
    :VISIBILITY: folded
    :END:
**** Summary
***** I
${I-info}
***** O
${O-info}
**** Logs
${logs}
**** Warnings
${warnings}")

(defconst tdbooster--template-attention
  "*** Logs
${logs}
*** Warnings
${warnings}")


(defun tdbooster--render-header (json transactions attentions)
  (let* ((name (tdbooster--name json))
	 (latest-trans (seq-first (tdbooster--translist json)))
	 (I-price-latest (or (tdbooster--trans-I-price (tdbooster--trans-I latest-trans)) "NIL"))
	 (O-price-latest (or (tdbooster--trans-O-price (tdbooster--trans-O latest-trans)) "NIL"))
	 (focus (if (< 0 (seq-length (tdbooster--attentionlist json))) "*" "-")))
    (s-format tdbooster--template-header 'aget `(("name" . ,name)
						 ("I-price-latest" . ,I-price-latest)
						 ("O-price-latest" . ,O-price-latest)
						 ("focus" . ,focus)
						 ("transactions" . ,transactions)
						 ("attentions" . ,attentions)))))

(defun tdbooster--render-transaction (json)
  (let* ((I-date (tdbooster--trans-I-date (tdbooster--trans-I json)))
	 (I-price (tdbooster--trans-I-price (tdbooster--trans-I json)))
	 (O-date (or (tdbooster--trans-O-date (tdbooster--trans-O json)) "NIL"))
	 (O-price (or (tdbooster--trans-O-price (tdbooster--trans-O json)) "NIL"))
	 (diff (if (equal  O-price "NIL") "NIL" (format "%.2f" (- O-price I-price))))
	 (I-info (tdbooster--trans-I-info (tdbooster--trans-I json)))
	 (O-info (or (tdbooster--trans-O-info (tdbooster--trans-O json)) "NIL"))
	 (logs (s-join "\n" (seq-map (lambda (log) (format "- %s" log)) (tdbooster--trans-logs json))))
	 (warnings (s-join "\n" (seq-map (lambda (warning) (format "- %s" warning)) (tdbooster--trans-warnings json)))))
    (s-format tdbooster--template-transaction 'aget `(("I-date" . ,I-date)
						      ("O-date" . ,O-date)
						      ("I-price" . ,I-price)
						      ("O-price" . ,O-price)
						      ("diff" . ,diff)
						      ("I-info" . ,I-info)
						      ("O-info" . ,O-info)
						      ("logs" . ,logs)
						      ("warnings" . ,warnings)))))


(defun tdbooster--render-attention (json)
  (let* ((logs (s-join "\n" (seq-map (lambda (log) (format "- %s" log)) (tdbooster--attention-logs json))))
	 (warnings (s-join "\n" (seq-map (lambda (warning) (format "- %s" warning)) (tdbooster--attention-warnings json)))))
    (s-format tdbooster--template-attention 'aget `(("logs" . ,logs)
						    ("warnings" . ,warnings)))))


(defun tdbooster--render-one (json)
  (let* ((trans-list-json (tdbooster--translist json))
	 (trans-list (s-join "\n" (seq-map (lambda (trans-json) (tdbooster--render-transaction trans-json))
					   trans-list-json)))
	 (attention-list-json (tdbooster--attentionlist json))
	 (attention-list (s-join "\n" (seq-map (lambda (attention-json) (tdbooster--render-attention attention-json))
					       attention-list-json)))
	 (all (tdbooster--render-header json trans-list attention-list)))
    all))

(defun tdbooster--render-all (json)
  (s-join "\n" (seq-map #'tdbooster--render-one json)))


(define-derived-mode tdbooster-mode org-mode "tdbooster"
  "major mode for tdbooster"
  (read-only-mode))

(defun tdbooster (refreshdata strategy)
  "Run tdbooster and show the results in tdbooster-mode."
  (interactive (list (yes-or-no-p "refresh datafile?") (completing-read "strategy: " '("long_term" "medium_term"))))
  (let* ((args (s-join " " (seq-map (lambda (s) (format "-f %s" s)) tdbooster-datafiles)))
	 (buffer (get-buffer-create (format "*tdbooster-%s*" strategy)))
	 (command (format "%s %s -s %s -o %s %s"  tdbooster-bin (if refreshdata "-r" "") strategy tdbooster-datafiles-dir args)))
    (with-current-buffer (get-buffer-create "*tdbooster-stdout*") (erase-buffer))
    (when (not (= 0 (call-process-shell-command command nil "*tdbooster-stdout*")))
      (error (format "something wrong with calling '%s', plz check '*tdbooster-stdout*' buffer" command)))
    (setq json-string (with-current-buffer "*tdbooster-stdout*" (buffer-string)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (tdbooster--render-all (json-read-from-string json-string)))
      (tdbooster-mode))
    (select-window
     (display-buffer
      buffer
      '((display-buffer-use-some-window))))))

(provide 'tdbooster)
;;; tdbooster.el ends here
