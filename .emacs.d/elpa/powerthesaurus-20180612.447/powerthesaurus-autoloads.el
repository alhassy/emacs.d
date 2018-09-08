;;; powerthesaurus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "powerthesaurus" "powerthesaurus.el" (23352
;;;;;;  11260 309013 524000))
;;; Generated autoloads from powerthesaurus.el

(autoload 'powerthesaurus-lookup-word-at-point "powerthesaurus" "\
Find word at `WORD-POINT', look it up in powerthesaurs, and replace it.

\(fn WORD-POINT)" t nil)

(autoload 'powerthesaurus-lookup-word "powerthesaurus" "\
Find the given word's synonyms at powerthesaurus.org.

`BEGINNING' and `END' correspond to the selected text with a word to replace.
If there is no selection provided, additional input will be required.
In this case, a selected synonym will be inserted at the point.

\(fn &optional BEGINNING END)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; powerthesaurus-autoloads.el ends here
