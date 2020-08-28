;; run follow codes at project root directory to format all ml,mli files
(dolist (f (directory-files-recursively "." "\.mli?$" nil))
  (unless (s-contains? "_build/" f)
    (with-current-buffer (find-file-noselect f)
      (ocamlformat)
      (save-buffer))))
