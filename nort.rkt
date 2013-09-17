;; noter  (C) Stephen De Gabrielle GPL 2 +
(module noter mzscheme
  (require 
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   ; reserved for future expansion
   (lib "class.ss")
   (lib "list.ss")
   (lib "pregexp.ss"))
  
  ;;;;;;;;;;;;;;;;;;;;
  ;; helper methods ;;  
  ;;;;;;;;;;;;;;;;;;;;
  
  ;; pregexp-match : query -> string-to-match
  (define (pregexp-match-string-foldcase query string-to-match)
    (pregexp-match (string-foldcase query) (string-foldcase string-to-match)))
  
  ;; word-filter: pre-word -> List of matching terms
  ;; rerturns a list of strings that contain the query 
  (define (word-filter query terms)
    (filter (lambda (string-to-match) (pregexp-match-string-foldcase query string-to-match)) terms)) 
  ;;
  ;; splitter : string -> list of strings
  ;; splitter ; split string on ANY whitespace
  (define (splitter a-string)
    (pregexp-split "\\s+" a-string))
  ;;
  (define (strip-non-alphanumerical a-string) ; improve this
    (pregexp-replace* "\\s+$" 
                      ;; remove trailing space
                      (pregexp-replace* "\\s+" ; remove multiple spaces
                                        ; remove non alphanum 
                                        (pregexp-replace* "[^[:alnum:]]+" a-string " ") " ") "")) 
  
  ;; stem: word-string -> stem-string
  ;; primitive stemmer the removes, s er, es, ing, ion, ed from the input string
  (define (stem a-string)
    void)
  
  
  ;; sorter:  a-list-of-strings ->  a-list-of-strings
  ;; sorts list in a case insensitive/alphabetical manner
  (define (sorter a-list-of-strings)
    (sort a-list-of-strings string-ci<?))
  ;;
  
  ;; applies the procedure proc to each element in hash-table, accumulating the results into a list.
  ;; The procedure proc must take two arguments: a key and its value. See the caveat below about concurrent modification.
  
  (define ... '())
  ;; import-file: pathname file-description -> void 
  ;; appends text to matching description   
  (define (import-file pathname file-description)
    ...)
  ;; import-directory: path path-description -> void
  ;; recurses over directory and children - sending every file found to import-file
  (define (import-directory path path-description)
    ...)
  
  ;;; app specific
  
  ;; make-new-document: description -> void
  ; makes a new editor (text%) with autowrap #t and modified keymap
  ; updates the textdb global hash with the description as the key and the new %text as the value.
  ; This should be in new class definition
  (define (make-new-document description)
    (let* ; create new text object (note)
        ((the-new-text (instantiate text% ()))
         (the-keymap (send the-new-text get-keymap)))
      (send the-new-text auto-wrap #t)
      (keymap:setup-global the-keymap)
      (send the-keymap add-function "Return to text-field-object" 
            (lambda (text-field-object x) (send the-text-field focus)))
      (send the-keymap map-function "c:q" "Return to text-field-object")
      (hash-table-put! textdb description the-new-text)
      ))
  
  
  (define (noter-import)
    (void))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; DEFINE THE DB - HASHTABLE USED  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (define textdb (make-hash-table 'equal))
  ;; key used is the description string
  ;; value is a text% object
  
  ;; matcher: description hashtable -> matching keys
  ;; split description, find keys that match description 
  ;;
  (define (matcher description hashtable)
    (let ((terms (splitter description)); terms from the query
          (list-of-keys (hash-table-map textdb (lambda (k v) k )))
          ) 
      (sorter (filter (lambda (k) ; sorted  list of documents that are the result of a logical and of search terms
                        (andmap (lambda (t) (pregexp-match-string-foldcase t k))
                                terms) ; return #f for not include
                        ) list-of-keys))
      ))
  
  ;; messages/methods required
  
  ;(list-of-keys (hash-table-map textdb (lambda (k v) k )))
  
  ;(current-text (hash-table-get textdb description #f))
  ;(send the-list-box set (matcher description textdb)) ; update list-box to show all matching
  ;(send the-list-box set (matcher description textdb))
  
  ;(send the-editor-canvas set-editor (hash-table-get textdb description #f))
  ;(send the-editor-canvas set-editor (hash-table-get textdb description #f))))
  
  ;(open-output-text-editor (hash-table-get textdb description #f)) ;print to outport port
  ;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;
  ;; callbacks ;;
  ;;;;;;;;;;;;;;;   
  
  (define (the-text-field-callback text-field-object ce) ; ce:control-event-returned
    (let ((description (send text-field-object get-value))
          (this-event (send ce get-event-type)))
      (cond
        [(eq? this-event 'text-field-enter) ; create a new note or open existing one.
         (let ; is it a new description?
             ((current-text (hash-table-get textdb description #f))) ; returns #f if a new key
           (cond
             [(eq? current-text #f) ;if a new key
              (make-new-document description) ; make a new text% in textdb with key description
              (send the-list-box set (matcher description textdb)) ; update list-box to show all matching
              ;(send the-list-box append description); add description to list box
              (send the-list-box set-string-selection description) ;and make current selection
              (send the-editor-canvas set-editor (hash-table-get textdb description #f))
              (send the-editor-canvas focus)
              ]
             [else
              (begin ; (open)switch to matching text object(note)
                (send the-editor-canvas set-editor current-text)
                (send the-editor-canvas focus))
              ;;
              ]))]
        [else (begin
                (send the-list-box set (matcher description textdb)) ;;
                ;; (printf "~a~n" description )
                )])) ; not 'text-field-enter - search (interactive match) and auto-completion go here
    1)
  
  
  (define (the-list-box-callback list-box-object ce) ; ce:control-event-returned
    
    (let ((this-event (send ce get-event-type)); always (eq? this-event 'list-box) 
          (description (send list-box-object get-string-selection))); get-data (car (send list-box-object get-selections)))))
      ; (printf "called ~a ~a ~n" (hash-table-get textdb description #f) description) ;
      (send the-text-field set-value description) 
      (send the-editor-canvas set-editor (hash-table-get textdb description #f))))
  
  ;; dropped-file-callback : path -> void
  ;; called when a file or directory is dropped on noter
  (define (dropped-file-callback path)
    (let ((path-description (strip-non-alphanumerical (path->string path)))) ;; use path as default description
      (cond 
        [(file-exists? path) ;; refers to a specific file 
         (let
             ((description (path->string path))) ; is this still needed?
           
           (send the-text-field set-value path-description)
           (the-text-field-callback the-text-field (new control-event% (event-type 'text-field-enter)))
           ;;  send copy of file to text-editor
           (send (hash-table-get textdb path-description #f) load-file path 'guess #f) 
           ;           (fprintf (open-output-text-editor (hash-table-get textdb path-description #f)) ;print to outport port
           ;                    (call-with-input-file path ;; mzscheme accepts path datatype
           ;                      (λ (input-port) (read-string 1000 input-port)) 'text))   ; data to print from file
           )]
        [(directory-exists? path)   ;; refers to a directory
         (import-directory path path-description)]
        
        [else    
         (begin
           (send the-text-field focus)
           (send the-text-field set-value (path->string path)) 
           )])
      (printf "~a~n" path)))
  
  ;;;;;;;;;;;;;;;;;;;;;
  ;; GUI definition  ;;
  ;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;
  ;; Classes         ;;
  ;;;;;;;;;;;;;;;;;;;;;
  (define a-drop-frame%
    (class frame:standard-menus%
      (define/override (on-drop-file e) 
        (dropped-file-callback e)
        (super on-drop-file e))
      (super-instantiate ()))) 
  
  (define a-rich-text%
    (class frame:standard-menus%
      (define/override (on-drop-file e) 
        (dropped-file-callback e)
        (super on-drop-file e))
      (super-instantiate ()))) 
  
  
  ;text:basic%
  
  
  ;;;;;;;;;;;;;;;;;;;;;
  ;; Objects         ;;
  ;;;;;;;;;;;;;;;;;;;;;
  (define the-the-frame
    (new a-drop-frame%
         (label "Noter - with incremental")
         (width 600)
         (height 600)))
  
  (define the-frame (send the-the-frame get-area-container))
  
  (send the-frame accept-drop-files #t)
  ;; Add the 'description' text field 
  (define the-text-field (instantiate text-field% (#f the-frame the-text-field-callback)))
  
  
  (define the-vertical-pane
    (new panel:vertical-dragable%
         (parent the-frame)))
  ;; add the matching descriptions list box
  (define the-list-box
    (instantiate list-box% ("" '("") the-vertical-pane the-list-box-callback)
      (stretchable-height #f)
      (min-height 80)))
  
  (define the-editor-canvas (instantiate canvas:basic% (the-vertical-pane)))
  ; (send the-editor-canvas accept-drop-files #t)
  ; (send the-editor-canvas allow-tab-exit #t) 
  
  ;(define the-pasteboard (instantiate pasteboard% ()))
  ;(send the-editor-canvas set-editor the-pasteboard)
  
  ;(send the-editor-canvas set-editor the-text)
  
  ;(define the-menu-bar (instantiate menu-bar% (the-frame)))
  ;(define the-edit-menu (instantiate menu% ("Edit" the-menu-bar)))
  ;(append-editor-operation-menu-items the-edit-menu #f)
  
  (send the-the-frame show #t) 
  )
