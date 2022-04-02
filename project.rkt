#lang racket
(require loudhum)
(require (prefix-in htdp: 2htdp/image))
(require racket/gui/base)
(require plot)

;;; Project Title:
;;;   A Search Engine: Grinnell in the 1960’s
;;; Authors:
;;;   Arsema Berhane
;;;   Philip Phuc
;;;   Kaleb Shah
;;;   Sumin Goh

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  gui start  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Name:
;;;   result-options
;;; Description:
;;;   a list of strings "1", ..., "20"
(define result-options
  (map number->string (drop (range 21) 1)))      

;;; Name:
;;;   search-engine
;;; Description:
;;;   the main frame
(define search-engine
  (new frame%
       [label "S&B Search"]
       [height 1000]
       [width 700]))

;;; Name:
;;;   vert-panel
;;; Description:
;;;   the vertical panel that organizes elements top-down
(define vert-panel
  (new vertical-panel%
       [parent search-engine]
       [alignment '(center top)]))     

;;; Name:
;;;   title-pane
;;; Description:
;;;   a pane containing the text "Scarlet & Black search"
(define title-pane
  (new pane%
       [parent vert-panel]
       [vert-margin 0]
       [horiz-margin 0]
       [border 0]
       [spacing 0]
       [alignment '(center center)]
       [min-width 500]
       [min-height 300]
       [stretchable-height 350]))

;;; Description:
;;;   a canvas that draws the text "Scarlet & Black search"
(new canvas%
     [parent title-pane]
     [style '(transparent)]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 1 1)
        (send dc set-text-foreground "Firebrick")
        (send dc set-font (make-font #:size 60
                                     #:family 'roman
                                     #:weight 'bold))
        (send dc draw-text "Scarlet" 230 130)
        (send dc set-text-foreground "White")
        (send dc set-font (make-font #:size 60
                                     #:family 'roman
                                     #:weight 'bold))
        (send dc draw-text "&" 280 185)
        (send dc set-text-foreground "Black")
        (send dc set-font (make-font #:size 60
                                     #:family 'roman
                                     #:weight 'bold))
        (send dc draw-text "Black" 330 180)
        (send dc set-text-foreground "Black")
        (send dc set-font (make-font #:size 25
                                     #:family 'modern
                                     #:weight 'thin))
        (send dc draw-text "search" 282 230))])

;;; Name:
;;;   horiz-panel
;;; Description:
;;;   a panel that organizes objects horizontally
(define horiz-panel
  (new horizontal-panel%
       [parent vert-panel]
       [horiz-margin 110]
       [alignment '(center center)]
       [min-width 90]
       [min-height 15]
       [stretchable-width 100]
       [stretchable-height 20]
       [spacing 5]))

;;; Name:
;;;   inquiry
;;; Description:
;;;   a text field into which the user types the search keyword
(define inquiry
  (new text-field%
       [parent horiz-panel]
       [font (make-object font% 15 'modern 'italic 'light
               #f 'default #f 'aligned)]
       [init-value "Enter a keyword"]
       ;[min-width 90]
       ;[min-height 15]
       ;[stretchable-width 100]
       ;[stretchable-height 20]
       [label ""]))

;;; Name:
;;;   result-num
;;; Description:
;;;   a drop-down list of numbers 1-20; for choosing the number of results
;;;   wanted
(define result-num
  (new choice%
       [parent horiz-panel]
       [label "# results"]
       [choices result-options]))

;;; Name:
;;;   result-pane
;;; Description:
;;;   a pane that contains the results of the search
(define result-pane
  (new pane%
       [parent vert-panel]
       [vert-margin 30]
       [horiz-margin 70]
       [border 51]
       [spacing 0]
       [alignment '(center top)]
       [min-width 500]
       [min-height 400]
       [stretchable-width 600]
       [stretchable-height 500]))

;;; Procedure:
;;;   open-file
;;; Parameters:
;;;   a
;;;   b
;;; Purpose:
;;;   Opens the issue that the user clicked
;;; Produces:
;;;   [None]
(define open-file
  (lambda (a b)
    (let ([selections (send result-list-box get-selections)])
      (when (not (null? selections))
        (let* ([date (substring (send result-list-box
                                      get-string
                                      (car selections))
                                0 10)]
               [file-name (string-append
                           "improved/"
                           date
                           "-improved")])
          (define new-window
            (new frame%
                 [label (string-append "S&B " date)]
                 [height 800]
                 [width 600]))
          (define t (new text%))
          (new editor-canvas%
               [parent new-window]
               [editor t])
          (send t insert (file->string file-name))
          (send t scroll-to-position 0 #f 'same 'none)
          (send t lock #t)
          (send new-window show #t))))))

;;; Name:
;;;   result-list-box
;;; Description:
;;;   a list box that lists the results in order
(define result-list-box
  (new list-box%
       [style '(single vertical-label)]
       [parent result-pane]
       [choices null]
       [label ""]
       [callback open-file]))

;;; Procedure:
;;;   show-result
;;; Parameters:
;;;   a
;;;   b
;;; Purpose:
;;;   Shows the results in the result-list-box
;;; Produces:
;;;   [None]
(define search-clicked
  (lambda (a b)
    (let ([keyword (send inquiry get-value)])
      (when (not (equal? "" keyword))
        (let* ([result (search-list
                        keyword
                        (add1 (send result-num get-selection)))]
               [result-list
                (map (section string-append
                              <>
                              "  --  number of \"" keyword "\" in this issue: "
                              <>)
                     (map car result)
                     (map number->string (map cdr result)))])
          (when (not (null? (send result-list-box get-selections)))
            (send result-list-box select
                  (car (send result-list-box get-selections)) #f))
          (send result-list-box set result-list))))))

;;; Description:
;;;   the search button that triggers show-result
(new button%
     [parent horiz-panel]
     [label "Search"]
     [callback search-clicked])

;;; Purpose:
;;;   displays the search-engine frame
(send search-engine show #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  gui end  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Name:
;;;   dict466547
;;; Description:
;;;   a list of 466547 words in the dictionary
(define dict466547
  (string-split
   (file->string "dict466547.txt")
   "\n"))

;;; Name:
;;;   dict10000
;;; Description:
;;;   a list of 10000 words in the dictionary
(define dict10000
  (string-split
   (file->string "dict10000.txt")
   "\n"))

;;; Name:
;;;   dict3000
;;; Description:
;;;   a list of 3000 words in the dictionary
(define dict3000
  (string-split
   (file->string "dict3000.txt")
   "\n"))

;;; Name:
;;;   dict1000
;;; Description:
;;;   a list of 1000 words in the dictionary
(define dict1000
  (string-split
   (file->string "dict1000.txt")
   "\n"))

;;; Name:
;;;   dict-names-466547
;;; Description:
;;;   a vector of file names "dict0", ... , "dict19" for dict466547
(define dict-names-466547
  (apply vector (map (o (section string-append "dict466547_str_lengths/dict" <>)
                        (section number->string <>))
                     (range 20))))

;;; Name:
;;;   dict-names-10000
;;; Description:
;;;   a vector of file names "dict0", ... , "dict19" for dict10000
;;; Citation:
;;;   https://github.com/first20hours/google-10000-english/blob/master/google-\
;;;   10000-english.txt
(define dict-names-10000
  (apply vector (map (o (section string-append "dict10000_str_lengths/dict" <>)
                        (section number->string <>))
                     (range 20))))

;;; Name:
;;;   dict-names-3000
;;; Description:
;;;   a vector of file names "dict0", ... , "dict19" for dict3000
(define dict-names-3000
  (apply vector (map (o (section string-append "dict3000_str_lengths/dict" <>)
                        (section number->string <>))
                     (range 20))))

;;; Name:
;;;   dict-names-1000
;;; Description:
;;;   a vector of file names "dict0", ... , "dict19" for dict1000
(define dict-names-1000
  (apply vector (map (o (section string-append "dict1000_str_lengths/dict" <>)
                        (section number->string <>))
                     (range 20))))

;;; Procedure:
;;;   make-cleaned-dicts
;;; Parameters:
;;;   dict, a list of strings
;;;   folder-name, a string
;;; Purpose:
;;;   Makes the files "dict0", ... , "dict19", each of which contains only words
;;;   whose lengths vary between 0 and 19 letters
;;; Produces:
;;;   [None]
(define make-cleaned-dicts
  (lambda (dict folder-name)
    (let* ([sorted-by-length (sort dict (lambda (str1 str2)
                                          (< (string-length str1)
                                             (string-length str2))))]
           [length-lists (map (lambda (num)
                                (filter (lambda (str)
                                          (equal? (string-length str) num))
                                        sorted-by-length))
                              (range 20))])
      (for-each (lambda (lst num)
                  (string->file (if (null? lst)
                                    ""
                                    (reduce (section string-append <> "\n" <>)
                                            lst))
                                (string-append
                                 folder-name
                                 "/dict" (number->string num))))
                length-lists
                (range 20)))))

;;; Procedure:
;;;   match-words
;;; Parameter:
;;;   original, a string
;;; Purpose:
;;;   Fixes the spelling of original if original does not exist in the
;;;   dictionary
;;; Produces:
;;;   result, a string representing the correctly-spelled version of original
(define match-words
  (lambda (original)
    (if (or (list? (member (string-downcase original)
                           (map string-downcase dict10000)))
            (> (string-length original) 19))
        original
        (let* ([original-len (string-length original)]
               [match-database (file->lines
                                (vector-ref dict-names-10000
                                            original-len))])  
          (match-words-helper
           original
           original
           match-database
           (quotient (string-length original) 2))))))

;;; Procedure:
;;;   match-words-helper
;;; Parameter:
;;;   original, a non-empty string containing only letters
;;;   closest-so-far, a string containing only letters
;;;   remaining, a list of strings
;;;   threshold, an exact non-negative integer
;;; Purpose:
;;;   Fixes the spelling of original if original does not exist in the
;;;   dictionary
;;; Produces:
;;;   matched, a correctly-spelled version of original
(define match-words-helper
  (lambda (original closest-so-far remaining threshold)
    (if (null? remaining)
        closest-so-far
        (let* ([word (car remaining)]
               [num-matches (length
                             (remove*
                              (list #f)
                              (map equal?
                                   (string->list
                                    original)
                                   (string->list
                                    word))))])
          (cond
            [(> num-matches threshold)
             (match-words-helper
              original
              word
              (cdr remaining)
              num-matches)]
            [else
             (match-words-helper
              original
              closest-so-far
              (cdr remaining)
              threshold)])))))

;;; Procedure:
;;;   improve-ocr
;;; Parameter:
;;;   file-name, a string
;;; Purpose:
;;;   Improves the OCR text
;;; Produces:
;;;   result, a string representing the improved version of str
(define improve-ocr
  (lambda (date)
    (let* ([step0 (file->string (string-append "issues/" date))]
           [step1 (regexp-replace* #px"(-\n|~\n|€â€œ)(\\w+)"
                                   step0
                                   "\\2")]
           [step2 (regexp-replace* #px"(\n)\n+" step1 "\\1")]
           [step3 (regexp-replace* #px"[a-zA-Z]+"
                                   step2
                                   match-words)])
      (string->file step3
                    (string-append
                     "improved/"
                     date
                     "-improved")))))

;;; Procedure:
;;;   all-alphabet?
;;; Parameters:
;;;   str, a string 
;;; Purpose:
;;;   Returns true if all characters of the string are alphabets
;;; Produces:
;;;   boolean, #t if all the characters in a string are alphabets
;;;   #f if the string has non-alphabet characters
(define all-alphabet?
  (lambda (str)
    (equal? str (car (regexp-match #px"[a-zA-Z ]*" str))))) 

;;; Procedure:
;;;   capitalize
;;; Parameters:
;;;   str, a string 
;;; Purpose:
;;;   Capatilizes the first alphabetg of every word in a string.
;;; Produces:
;;;   result, a string
(define capitalize
  (lambda (str)
    (if (all-alphabet? str)
        (let* ([first-alph
                (map string-upcase
                     (regexp-match* #px"^.| ." str))]
               [remaining-words
                (string-split
                 (regexp-replace* #px"^.| ." str "!@$")
                 "!@$")]
               [list1 (map string-append first-alph remaining-words)])
          (string-append "|" (reduce string-append list1)))
        "")))      

;;; Procedure:
;;;   string-maker
;;; Parameters:
;;;   str, a string 
;;; Purpose:
;;;   Appends str, uppercase of str, lowercase of str and capitaliz str with
;;;   "|" between every version.
;;; Produces:
;;;   result, a string
(define string-maker
  (lambda (str)
    (string-append
     str "|"
     (string-upcase str) "|"
     (string-downcase str)
     (capitalize str))))

;;; Procedure:
;;;   search
;;; Parameter:
;;;   str, a string
;;; Purpose:
;;;   Returns the dates of all issues in which str appears
;;; Produces:
;;;   searched, a list of strings
;;; Preconditions:
;;;   * str is comprised of letters and is case-sensitive
;;; Postconditions:
;;;   * If str is not in any S&B issues, searched is null
;;;   * Otherwise, searched is a list of strings in “yyyy_mm_dd” format
(define search
  (lambda (str)
    (let kernel ([issues-list null]
                 [remaining dates])
      (cond
        [(null? remaining)
         issues-list]
        [(regexp-match? str (file->string
                             (string-append "improved/"
                                            (car remaining)
                                            "-improved")))
         (kernel (cons (car remaining) issues-list) (cdr remaining))]
        [else
         (kernel issues-list (cdr remaining))]))))

;;; Procedure:
;;;   search-list-recent
;;; Parameters:
;;;   str, a string representing the search keyword
;;;   n, a positive integer representing the number of results desired
;;; Purpose:
;;;   Presents n number of most recent issues containing the keyword str
;;; Produces:
;;;   result, a list of pairs '(pair-1 ... pair-n) in which (car pair-n) is the
;;;   date of the issue and (cdr pair-n) is the number of times str appears in
;;;   the issue
(define search-list-recent
  (lambda (str n)
    (let kernel ([remaining dates]
                 [results null])
      (cond
        [(null? remaining)
         (let ([sorted (sort results more-recent?)])
           (if (> n (length sorted))
               sorted
               (take sorted n)))]
        [(regexp-match? (string-maker str) (file->string
                                            (string-append
                                             "improved/"
                                             (car remaining)
                                             "-improved")))
         (kernel
          (cdr remaining)
          (cons (cons (car remaining)
                      (length
                       (regexp-match* (string-maker str)
                                      (file->string
                                       (string-append
                                        "improved/"
                                        (car remaining)
                                        "-improved")))))
                results))]
        [else
         (kernel (cdr remaining) results)]))))

;;; Procedure:
;;;   search-list
;;; Parameters:
;;;   str, a string representing the search keyword
;;;   n, the number of desired results
;;; Purpose:
;;;   Searches all the S&B issues with keyword str and returns n number of
;;;   most-relevant results
;;; Produces:
;;;   result, a list of pairs '(pair-1 ... pair-n) in which (car pair-i) is the
;;;   date of the issue and (cdr pair-i) is the number of times str appears in
;;;   the issue
(define search-list
  (lambda (str n)
    (let kernel ([remaining dates]
                 [results null])
      (if (null? remaining)
          (let ([sorted (sort results more-frequent?)])
            (if (> n (length sorted))
                sorted
                (take sorted n)))
          (let ([match (regexp-match* (string-maker str)
                                      (file->string
                                       (string-append
                                        "improved/"
                                        (car remaining)
                                        "-improved")))])
            (if (null? match)
                (kernel (cdr remaining) results)
                (kernel (cdr remaining)
                        (cons (cons (car remaining) (length match))
                              results))))))))

;;; Procedure:
;;;   improve-issues
;;; Parameters:
;;;   issues, a list of strings representing issues
;;; Purpose:
;;;   runs improve-ocr on issues
;;; Produces:
;;;   [None]
;cpu time: 9013834 real time: 15705200 gc time: 2166127
;4.36 hours
(define improve-issues
  (lambda ()
    (for-each
     (section improve-ocr <>)
     dates)))

;;; Procedure:
;;;   make-issues
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Combines the S&B files for each date into a single issue
;;; Produces:
;;;   [None]
(define make-issues
  (lambda ()
    (for-each (section combine-rec <>) dates)))

;;; Procedure:
;;;   add-zeros
;;; Parameters:
;;;   num, a non-negative real number
;;;   wanted-length, an exact non-negative integer
;;; Purpose:
;;;   Creates a string whose length is wanted-length
;;; Produces:
;;;   added, a string
;;; Preconditions:
;;;   * num is exact
;;; Postconditions:
;;;   * add-zeros adds (- wanted-length (string-length num)) zero’s to num
;;;   * (length added) = wanted-length
(define add-zeros
  (lambda (num wanted-length)
    (string-append
     (make-string (- wanted-length
                     (+ 1 (inexact->exact (floor (log num 10))))) #\0)
     (number->string num))))

;;; Procedure:
;;;   combine-rec
;;; Parameter:
;;;   date, a string
;;; Purpose:
;;;   Combines all pages from the issue published on date
;;; Produces:
;;;   combined, a file
;;; Preconditions:
;;;   * date is in “yyyy_mm_dd” format
;;;   * 1961 =< (string->number (substring date 0 4)) =< 1970
;;;   * 01 =< (string-number (substring date 5 7)) =< 12
;;;   * 01 =< (substring 8 10) =< 31
;;; Postconditions:
;;;   [No additional]
(define combine-rec
  (lambda (date); test date: 1961_01_13
    (let kernel ([page 1]
                 [so-far ""])
      ;(print (list 'kernel page)) (newline)
      (cond
        [(not (file-exists? (file-name date page)))
         (string->file so-far (string-append "issues/" date))]
        [else
         (kernel (add1 page)
                 (string-append
                  so-far
                  (file->string
                   (file-name date page))))]))))

;;; Procedure:
;;;   file-name
;;; Parameters:
;;;   date, a string
;;;   page, an exact non-negative integer
;;; Purpose:
;;;   Creates an appropriate link to the file with date and page
;;; Produces:
;;;   linked, a string
;;; Preconditions:
;;;   * date is in “yyyy_mm_dd” format
;;;   * 1961 =< (string->number (substring date 0 4)) =< 1970
;;;   * 01 =< (string-number (substring date 5 7)) =< 12
;;;   * 01 =< (substring 8 10) =< 31
;;;   * 1 =< num =< 10
;;; Postconditions:
;;;   [No additional]
(define file-name
  (lambda (date page)
    (string-append
     "original_files/"
     "usiagrc_scb_"
     date "_50_000_" (add-zeros page 5)
     "-00000_000.txt.txt")))

;;; Procedure:
;;;   more-frequent?
;;; Parameters:
;;;   pair-1, a pair
;;;   pair-2, a pair
;;; Purpose:
;;;   Compares pair-1 and pair-2 based on each of their cdrs, the frequency of a
;;;   keyword in that issue; if (= (cdr pair-1) (cdr pair-2)), more-recent? gets
;;;   called instead
;;; Produces:
;;;   result, a boolean
(define more-frequent?
  (lambda (pair-1 pair-2)
    (cond [(> (cdr pair-1) (cdr pair-2))
           #t]
          [(< (cdr pair-1) (cdr pair-2))
           #f]
          [else
           (more-recent? pair-1 pair-2)])))

;;; Procedure:
;;;   more-recent?
;;; Parameters:
;;;   pair-1, a pair
;;;   pair-2, a pair
;;; Purpose:
;;;   Compares pair-1 and pair-2 based on each of their cars, the date
;;; Produces:
;;;   result, a boolean
(define more-recent?
  (lambda (pair-1 pair-2)
    (let ([date1 (string->date (car pair-1))]
          [date2 (string->date (car pair-2))])
      (cond [(> (date-year date1) (date-year date2))
             #t]
            [(< (date-year date1) (date-year date2))
             #f]
            [(> (date-month date1) (date-month date2))
             #t]
            [(< (date-month date1) (date-month date2))
             #f]
            [(> (date-day date1) (date-day date2))
             #t]
            [(< (date-day date1) (date-day date2))
             #f]
            [else
             #t]))))

;;; Name:
;;;   month-hash
;;; Description:
;;;   a hash table with the numeric representation of the months as the keys and
;;;   the alphabetical representation of the months as the values
(define month-hash
  (make-hash (map cons
                  (list "01" "02" "03" "04" "05" "06"
                        "07" "08" "09" "10" "11" "12")
                  (list "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))))

;;; Procedure:
;;;   num->month
;;; Parameter:
;;;   str, a string in one of the following forms, "01", "02", ..., "12"
;;; Purpose:
;;;   changes the numeric representation of a month into a alphabetical one
;;; Produces:
;;;   result, the alphabetical representation of the month str
(define num->month
  (lambda (str)
    (hash-ref month-hash str)))

;;; Name:
;;;   date-kernel
;;; Description:
;;;   a struct representing a date with the year, month, and day
(struct date-kernel (year month day) #:transparent)

;;; Procedure:
;;;   date
;;; Parameters:
;;;   year, an integer between 1961 and 1970, inclusive
;;;   month, an integer between 1 and 12, inclusive
;;;   day, an integer between 1 and 31, inclusive
;;; Purpose:
;;;   Checks if the given parameters can form a proper date, and if so, calls
;;;   date-kernel
;;; Produces:
;;;   [None]
(define date
  (lambda (year month day)
    (cond
      [(or (not (integer? year))
           (> year 1970)
           (< year 1961))
       (error "date: invalid year" year)]
      [(or (not (integer? month))
           (< month 1)
           (> month 12))
       (error "date: invalid month" month)]
      [(or (not (integer? day))
           (< day 1)
           (> day 31))]
      [else
       (date-kernel year month day)])))

(define date-year date-kernel-year)
(define date-month date-kernel-month)
(define date-day date-kernel-day)

;;; Procedure:
;;;   string->date
;;; Parameter:
;;;   str, a string representing a date
;;; Purpose:
;;;   Converts a date in a string form into a date struct
;;; Produces:
;;;   result, a date struct
(define string->date
  (lambda (str)
    (apply date (map string->number (string-split str "_")))))

;;; Procedure:
;;;   date->string
;;; Parameter:
;;;   dt, a date struct
;;; Purpose:
;;;   Converts a date struct into a string form
;;; Produces:
;;;   result, a string representing the date dt
(define date->string
  (lambda (dt)
    (string-append (number->string (date-year dt)) "_"
                   (add-zeros (date-month dt) 2) "_"
                   (add-zeros (date-day dt) 2))))

;;; Procedure:
;;;   file-names
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Changes the names of the files to the dates of the published articles
;;; Produces:
;;;   named, a list of all of the files 
(define file-names
  (lambda ()
    (let kernel ([issue-list null]
                 [year 1961]
                 [month 1]
                 [day 1]
                 [page 1])
      (let* ([date (string-append (number->string year) "_"
                                  (add-zeros month 2) "_"
                                  (add-zeros day 2))]
             [possible-file-name (string-append
                                  ; the below line can be commented out
                                  ;"/home/rebelsky/Desktop/SandB/"
                                  (file-name date page))]
             [new-date (vector year month day)])
        ; (write "possible-file-name: ")
        ; (write possible-file-name) (newline)
        ; for setting the new date
        (cond [(and (= 12 month) (> day 31))
               (vector-set! new-date 0 (add1 year))
               (vector-set! new-date 1 1)
               (vector-set! new-date 2 1)]
              [(> day 31)
               (vector-set! new-date 1 (add1 month))
               (vector-set! new-date 2 1)]
              [else
               (vector-set! new-date 2 (add1 day))])
        ; for running kernel
        (cond [(> (vector-ref new-date 0) 1970)
               issue-list]
              [(file-exists? possible-file-name)
               ; (write "possible-file-name exists") (newline)
               (kernel (cons possible-file-name issue-list)
                       year
                       month
                       day
                       (add1 page))]
              [else
               (kernel issue-list
                       (vector-ref new-date 0)
                       (vector-ref new-date 1)
                       (vector-ref new-date 2)
                       1)])))))

;;; Procedure:
;;;   extract-date
;;; Parameter:
;;;   file-name-lst, a list of all the file names
;;; Purpose:
;;;   Extracts all the dates of the S&B articles
;;; Produces:
;;;   result, a list of strings representing the dates of the S&B articles
(define extract-date
  (lambda (file-name-lst)
    (let* ([part1 (string-append
                   ;"/home/rebelsky/Desktop/SandB/"
                   "original_files/"
                   "usiagrc_scb_")]
           [part1-len (string-length part1)]
           [part2 "_50_000_00011-00000_000.txt.txt"]
           [part2-len (string-length part2)]
           [proc (lambda (str)
                   (substring str
                              part1-len
                              (- (string-length str) part2-len)))])
      (let kernel ([remaining file-name-lst]
                   [so-far null])
        (if (null? remaining)
            so-far
            (let ([date (proc (car remaining))])
              (cond
                [(and (not (null? so-far)) (equal? date (car so-far)))
                 (kernel (cdr remaining) so-far)]
                [else
                 (kernel (cdr remaining) (cons date so-far))])))))))

;;; All the dates of the S&B issues
(define dates
  '("1961_01_13" "1961_02_03" "1961_02_10" "1961_02_17" "1961_02_24"
                 "1961_03_03" "1961_03_10" "1961_03_17" "1961_03_24"
                 "1961_04_14" "1961_04_21" "1961_05_05" "1961_05_12"
                 "1961_05_19" "1961_06_02" "1961_09_15" "1961_09_22"
                 "1961_09_29" "1961_10_06" "1961_10_13" "1961_10_20"
                 "1961_10_27" "1961_11_03" "1961_11_10" "1961_11_17"
                 "1961_12_01" "1961_12_08" "1962_01_12" "1962_01_26"
                 "1962_02_02" "1962_02_09" "1962_02_16" "1962_02_23"
                 "1962_03_02" "1962_03_09" "1962_03_16" "1962_03_23"
                 "1962_04_13" "1962_04_20" "1962_04_27" "1962_05_02"
                 "1962_05_11" "1962_05_18" "1962_06_01" "1962_09_21"
                 "1962_09_28" "1962_10_05" "1962_10_12" "1962_10_19"
                 "1962_10_26" "1962_11_02" "1962_11_09" "1962_11_16"
                 "1962_11_23" "1962_11_30" "1962_12_07" "1962_12_14"
                 "1963_01_11" "1963_01_25" "1963_02_01" "1963_02_08"
                 "1963_02_15" "1963_02_22" "1963_03_01" "1963_03_08"
                 "1963_03_15" "1963_03_22" "1963_03_29" "1963_04_12"
                 "1963_04_19" "1963_04_26" "1963_05_03" "1963_05_10"
                 "1963_05_17" "1963_06_05" "1963_09_13" "1963_09_20"
                 "1963_09_27" "1963_10_04" "1963_10_11" "1963_10_18"
                 "1963_10_25" "1963_11_01" "1963_11_15" "1963_11_22"
                 "1963_12_06" "1963_12_13" "1964_01_10" "1964_01_24"
                 "1964_01_31" "1964_02_07" "1964_02_14" "1964_02_21"
                 "1964_02_28" "1964_03_06" "1964_03_13" "1964_03_14"
                 "1964_03_20" "1964_04_09" "1964_04_17" "1964_04_24"
                 "1964_05_01" "1964_05_08" "1964_05_15" "1964_05_22"
                 "1964_06_05" "1964_09_18" "1964_09_25" "1964_10_02"
                 "1964_10_09" "1964_10_16" "1964_10_23" "1964_10_30"
                 "1964_11_13" "1964_11_20" "1964_11_25" "1964_12_04"
                 "1964_12_11" "1965_01_08" "1965_01_22" "1965_02_05"
                 "1965_02_12" "1965_02_19" "1965_02_26" "1965_03_05"
                 "1965_03_12" "1965_03_20" "1965_03_26" "1965_04_09"
                 "1965_04_16" "1965_04_23" "1965_04_30" "1965_05_07"
                 "1965_05_14" "1965_05_21" "1965_06_04" "1965_09_10"
                 "1965_09_17" "1965_09_24" "1965_10_01" "1965_10_08"
                 "1965_10_15" "1965_10_22" "1965_11_05" "1965_11_12"
                 "1965_11_19" "1965_11_26" "1965_12_03" "1965_12_10"
                 "1966_01_21" "1966_01_28" "1966_02_04" "1966_02_11"
                 "1966_02_18" "1966_02_21" "1966_02_25" "1966_03_04"
                 "1966_03_11" "1966_04_01" "1966_04_08" "1966_04_15"
                 "1966_04_22" "1966_04_29" "1966_05_06" "1966_05_13"
                 "1966_05_27" "1966_09_03" "1966_09_09" "1966_09_16"
                 "1966_09_23" "1966_09_30" "1966_10_07" "1966_10_11"
                 "1966_10_14" "1966_10_21" "1966_10_28" "1966_11_09"
                 "1966_11_18" "1966_11_25" "1966_12_02" "1966_12_09"
                 "1967_01_20" "1967_01_27" "1967_02_03" "1967_02_10"
                 "1967_02_17" "1967_02_24" "1967_03_03" "1967_03_10"
                 "1967_03_17" "1967_04_07" "1967_04_14" "1967_04_22"
                 "1967_04_28" "1967_05_05" "1967_05_19" "1967_09_01"
                 "1967_09_08" "1967_09_15" "1967_09_22" "1967_09_29"
                 "1967_10_06" "1967_10_13" "1967_10_20" "1967_10_27"
                 "1967_11_01" "1967_11_17" "1967_11_24" "1967_12_01"
                 "1967_12_08" "1968_01_26" "1968_02_02" "1968_02_10"
                 "1968_02_16" "1968_03_01" "1968_03_08" "1968_04_12"
                 "1968_05_24" "1968_09_20" "1968_09_27" "1968_10_04"
                 "1968_10_11" "1968_10_18" "1968_11_01" "1968_11_08"
                 "1968_11_15" "1968_11_22" "1968_11_29" "1968_12_06"
                 "1968_12_11" "1969_01_24" "1969_01_31" "1969_02_07"
                 "1969_02_14" "1969_02_21" "1969_02_28" "1969_03_07"
                 "1969_04_04" "1969_04_11" "1969_04_18" "1969_04_25"
                 "1969_05_02" "1969_05_16" "1969_05_29" "1969_09_13"
                 "1969_09_19" "1969_09_27" "1969_10_03" "1969_10_10"
                 "1969_10_17" "1969_10_31" "1969_11_07" "1969_11_14"
                 "1969_11_21" "1969_12_05" "1969_12_12" "1970_01_23"
                 "1970_01_31" "1970_02_06" "1970_02_13" "1970_02_20"
                 "1970_02_27" "1970_03_06" "1970_03_13" "1970_04_03"
                 "1970_04_10" "1970_04_17" "1970_04_24" "1970_05_01"
                 "1970_05_08" "1970_05_15" "1970_10_16" "1970_10_23"
                 "1970_10_29" "1970_11_13" "1970_11_20" "1970_12_04"
                 "1970_12_11"))