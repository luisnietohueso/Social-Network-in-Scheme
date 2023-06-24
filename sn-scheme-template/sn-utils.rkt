(module sn-utils racket
  (provide sn-dict-ks-vs
           sn-line->entry
           sn-list->dict)

;; Returns a dictionary as a list of key-value pairs
;; sn-dict-ks-vs : list-of-any list-of-any -> list-of-(cons any any)
;; Given two lists `keys` and `values`, constructs a list of key-value pairs
;; by pairing corresponding elements from the two lists.
  (define (sn-dict-ks-vs keys values)
    (map cons keys values))

;; Returns a dictionary entry from a string, where the first word is the key and the rest are the value
;; sn-line->entry : string -> (cons symbol list-of-string)
;; Given a string `ln`, returns a dictionary entry where the first word is the key
;; and the rest are the value.
  (define (sn-line->entry ln)
  ; map through each split character and convert to a symbol
  (map
   (lambda (s) (string->symbol s))
   (string-split ln)))



;; Returns the input list of key-value pairs
;; sn-list->dict : list-of-(cons any any) -> list-of-(cons any any)
;; Given a list of key-value pairs `entries`, returns the same list.  
  (define (sn-list->dict entries)
    entries)
)
