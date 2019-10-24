


(require 'seq)
(require 'json)
(require 's)
(alist-get 'dt  '(I (dt . "dt") (prc . 1.23) (indayinfo (a1 . "a1") (a2 . "a2"))))

(setq a (json-read-file "test.json"))

(insert (format "%S" a))

(setq json  '((name . "xxxx") (trans . [((I (date . "dt") (price . 1.23) (info (a1 . "a1") (a2 . "a2"))) (O (date . "dt") (price . 1.23) (info (a1 . "a1") (a2 . "a2"))) (logs . ["log1" "log2" "log3"]))])) )



(setq json '((name . "xxxx") (trans . [((I (date . "dt") (price . 1.23) (info (a1 . "a1") (a2 . "a2"))) (O (date . "dt") (price . 1.23) (info (a1 . "a1") (a2 . "a2"))) (logs . ["log1" "log2" "log3"]))])))

(setq json '((I (date . "dt") (price . 1.23) (info (a1 . "a1") (a2 . "a2"))) (O (date . "dt") (price . 1.23) (info (a1 . "a1") (a2 . "a2"))) (logs . ["log1" "log2" "log3"])))


(defmacro tdbooster--name (json) `(alist-get 'name ,json))
(defmacro tdbooster--translist (json) `(alist-get 'trans ,json))
(defmacro tdbooster--trans-I (json) `(alist-get 'I ,json))
(defmacro tdbooster--trans-O (json) `(alist-get 'O ,json))
(defmacro tdbooster--trans-logs (json) `(alist-get 'logs ,json))
(defmacro tdbooster--trans-I-date (json) `(alist-get 'date ,json))
(defmacro tdbooster--trans-I-price (json) `(alist-get 'price ,json))
(defmacro tdbooster--trans-I-info (json) `(alist-get 'info ,json))
(defmacro tdbooster--trans-O-date (json) `(alist-get 'date ,json))
(defmacro tdbooster--trans-O-price (json) `(alist-get 'price ,json))
(defmacro tdbooster--trans-O-info (json) `(alist-get 'info ,json))


(defconst tdbooster--template-header
  "
* ${name} ${I-price-latest}/${O-price-latest}
** Transactions
")

(defconst tdbooster--template-transaction
  "*** ${I-date}/${I-price} - ${O-date}/${O-price} ${diff}
**** Summary
***** I
${I-info}
***** O
${O-info}
**** Logs
${logs}
")


(defun tdbooster--render-header (json)
  (let* ((name (tdbooster--name json))
	 (latest-trans (seq-first (tdbooster--translist json)))
	 (I-price-latest (tdbooster--trans-I-price (tdbooster--trans-I latest-trans)))
	 (O-price-latest (tdbooster--trans-O-price (tdbooster--trans-O latest-trans)))
	 )
    (s-format tdbooster--template-header 'aget `(("name" . ,name)
						 ("I-price-latest" . ,I-price-latest)
						 ("O-price-latest" . ,O-price-latest)))))

(defun tdbooster--render-transaction (json)
  (let* ((I-date (tdbooster--trans-I-date (tdbooster--trans-I json)))
	 (I-price (tdbooster--trans-I-price (tdbooster--trans-I json)))
	 (O-date (tdbooster--trans-O-date (tdbooster--trans-O json)))
	 (O-price (tdbooster--trans-O-price (tdbooster--trans-O json)))
	 (diff (- O-price I-price))
	 (I-info (tdbooster--trans-I-info (tdbooster--trans-I json)))
	 (O-info (tdbooster--trans-O-info (tdbooster--trans-O json)))
	 (logs (seq-reduce (lambda (r log-str) (concat r "\n" log-str))
			   (seq-map (lambda (log) (format "- %s" log)) (tdbooster--trans-logs json))
			   "")))
    (s-format tdbooster--template-transaction 'aget `(("I-date" . ,I-date)
						      ("O-date" . ,O-date)
						      ("I-price" . ,I-price)
						      ("O-price" . ,O-price)
						      ("diff" . ,diff)
						      ("I-info" . ,I-info)
						      ("O-info" . ,O-info)
						      ("logs" . ,logs)))))
