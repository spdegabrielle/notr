#lang racket/gui
;; noter  (C) Stephen De Gabrielle GPL 2 +
(require framework)
;;Noter; 
;
;Query/title box
;- typing searches and displays a list of matching notes (matched by title/query)
;- on enter creates new or edits existing text note
;- what does enter do to the altenate matches in the list box?
;list box
;- list of matching items based on current query
;- clicking an item selects that as the current note
;- this does not change the query to alow switching between matching notes
;test box
;- text of currently selected item
;- saved on every keypress
;- ?undo
;- pressing enter gets a new line
;? tab moves back to search box
;? shift tab back to list box

;; word-filter: pre-word -> List of matching terms
;; rerturns a list of strings that contain the query 
(define (word-filter query terms)
  (filter (lambda (string-to-match) (string-ci=? query string-to-match)) terms)) 

;; sorter:  a-list-of-strings ->  a-list-of-strings
;; sorts list in a case insensitive/alphabetical manner
(define (sorter a-list-of-strings)
  (sort a-list-of-strings string-ci<?))



(define notes-database% 
  (class* object% ((interface () new save load list))
    (super-new)
    ;; hash for testinG  -***  move to sqlite ASAP
    (define textdb (make-hasheq))
    ;; key used is the description string
    ;; value is a text string
    ;; new : title text -> title
    ;; create new entry in db
    (define/public (new title text) (hash-set! textdb title text) title)
    ;; save: title text -> title
    ;; update entry for title
    (define/public (save title text) (new title text)) ;; synonym with 
    ;; load : query-or-title -> 
    (define/public (load title) (hash-ref textdb title #f))
    ;; return a list of titles on notes dabase
    (define/public (list) (hash-map textdb (lambda (k v) k)))))

;;;;;;;;;;;;;;;;;;;;;
;; GUI definition  ;;
;;;;;;;;;;;;;;;;;;;;;

(define notr% 
  (class object%
    (define notes (new notes-database%))
    
    ;; matcher: description hashtable -> matching keys
    ;; split description, find keys that match description 
    (define (matcher querystring)
      (let ((terms (string-split querystring)); terms from the query
            (list-of-keys (send notes list))) 
        ; sorted  list of documents that are the result of a logical and of search terms
        (sorter (filter (lambda (k)
                          (andmap (lambda (t) (string-ci=? t k))
                                  terms)) list-of-keys))))
    
    ;; make-new-document: description -> void
    ; makes a new editor (text%) with autowrap #t and modified keymap
    ; updates the textdb global hash with the description as the key and the new %text as the value.
    ; This should be in new class definition
    (define (make-new-document description) void
      ;(let* ; create new text object (note)
      ;   (;(the-new-text (instantiate text% ()))
      ;(the-keymap (send the-new-text get-keymap)))
      ;(send the-new-text auto-wrap #t)
      ;    (keymap:setup-global the-keymap)
      ;    (send the-keymap add-function "Return to text-field-object" 
      ;          (lambda (text-field-object x) (send the-text-field focus)))
      ;    (send the-keymap map-function "c:q" "Return to text-field-object"))
      )
    
    ;; the-list-box-callback : list-box-object ce -> 
    ;;
    (define (the-list-box-callback list-box-object ce) ; ce:control-event-returned
      (let ((this-event (send ce get-event-type)); always (eq? this-event 'list-box) 
            (title (send list-box-object get-string-selection))
            (text-editor (send the-editor-canvas get-editor)))
        (send text-editor erase)  ;; clear 
        (send text-editor insert (send notes load title)) ;; and load
        ))
    
    (define (the-text-field-callback text-field-object ce) ; ce:control-event-returned
      (let ((query-or-name (send text-field-object get-value))
            (this-event (send ce get-event-type)))
        (cond
          [(eq? this-event 'text-field-enter) ; create a new note or open existing one.
           (let ; is it a new description?
               ((current-text (send notes load query-or-name))) ; returns #f if a new document
             (cond
               [(eq? current-text #f) ;there is no matching document so pressing enter creates a new document
                (let  ; (open)switch to matching text object(note)
                    ((editor (send the-editor-canvas get-editor current-text)))
                  (send the-editor-canvas focus)
                  (send editor erase)
                  (send the-list-box set (matcher query-or-name)) ; update list-box to show all matching keys
                  (send the-list-box append query-or-name); add description to list box
                  (send the-list-box set-string-selection query-or-name) ;and make current selection
                  )]
               [else  ;; current-text contains an existing text note
                (let ((editor (send the-editor-canvas get-editor current-text)))
                  (send the-editor-canvas focus)
                  (send editor erase)
                  (send editor insert current-text)
                  )
                ;;
                ]))]
          [else ;; some other key was pressed
           (begin
             (send the-list-box set (matcher query-or-name)) ;;
             ;; (printf "~a~n" description )
             )])) ; not 'text-field-enter - search (interactive match) and auto-completion go here
      1)
    
    
    ;;;;;;;;;;
    (super-new)
    (define editor 
      (new (class text% 
             (init-field on-change-callback)
             (inherit get-text)
             (super-new)
             (define (on-change)
               (on-change-callback (get-text))
               )
             (augment on-change)
             )
           (on-change-callback (λ (text)void) )
           ))
    ;;;;;;;;;;;;;;;;;;;;;
    ;; Objects         ;;
    ;;;;;;;;;;;;;;;;;;;;;
    (define notr-frame
      (new frame%
           (label "Noter - with incremental")
           (width 600)
           (height 600)))
    (send notr-frame show #t) 
    
    ;; Add the 'description' text field 
    (define the-text-field (new text-field% 
                                (label #f) 
                                (parent notr-frame)
                                (the-text-field-callback )))
    
    (define the-vertical-panel
      (new panel:vertical-dragable%
           (parent notr-frame)))
    ;; add the matching descriptions list box
    (define the-list-box
      (instantiate list-box% ("" '("") the-vertical-panel the-list-box-callback)
        (stretchable-height #f)
        (min-height 80)))
    (define the-editor-canvas (new editor-canvas%  
                                   (parent the-vertical-panel)
                                   [editor editor]))
    
    
    ))
(new notr%)