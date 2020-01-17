#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.12 2020-01-08 17:13:13-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))
		 
;;holds all functions (including operators)
; CAN'T BE CHANGD BY USER
(define *function-table* (make-hash))
(define (set-function! key value) (hash-set! *function-table* key value))
;initializing *function-table*
(for-each
    (lambda (pair) (set-function! (car pair) (cadr pair)))
    `(
        (abs, abs) (acos, acos) (asin, asin) (atan, atan)
        (ceiling, ceiling) (cos, cos)
        (exp, exp)
        (floor, floor)
        (log, log)
        (round, round)
        (sin, sin) (sqrt, sqrt)
        (tan, tan) (truncate, truncate)
        (+,+) (-,-) (*,*) (/,/) 
        (<,<) (>,> (<=,<=) (>=,>=))
        (^,expt)
    )
)

;;holds value of all variables (updated as needed)
;;if variable not found, return 0
;;table initialized w/ variables in "buildin symbols" section
(define *variable-table* (make-hash))
(define (set-variable! key value)
    (hash-set! *variable-table* key value))
;initilizing variable-table
(for-each
   (lambda (pair) (set-variable! (car pair) (cadr pair)))
    `( 
        (nan , (/ 0.0 0.0))
        (eof , 0.0)
        (pi  , (acos -1.0))
        (e   , (exp 1.0))
    )
)

;;holds all arrays defined in program
;;created with make-vector and updated with vector-set!
(define *array-table* (make-hash))

;;holds addresses of each line
(define *label-table* (make-hash))
(define (set-label! key value)
    (hash-set! *label-table* key value)
)

;goes to here from the write-program-line function
(define (print-statement line)
	(if (null? (cdr line))
		(printf "Nothing to print from this line ~n")            
        ;when not just line number...
		(when not (=(length(cdr line))0) 
            ;if first command is print... execute the print
            ;caadr = function name (print, let, etc)
            ;cdadr = the other stuff thats important
            (when (eqv? (caadr line) 'print)
                (execute-the-print (cdadr line))
            )
		)
	)

)
 
;goes to here from print-statement function
;line = everything after 'print'
(define (execute-the-print line)
    ;if have something to print out
    (when (> (length line) 0)
        ;(printf "length of line is: ~s~n" (length line))
        (printf "~s ~n" line)

        ;if string, just print it out
        (when (string? (car line))
            (display (car line))
            (newline)	
        )
        ;if number, add 0.0 to make real number
        (when (number? (car line))
            (printf "is number~n")
        ) 
 
        ;if is a pair ex. (+ 2 4) 
        (when (pair? (car line)) 
            (printf "is pair~n")
            ;(define first (caar line))
            (if (hash-has-key? *function-table* (caar line))
                (printf "is in function-hash ~n")
                 ;(printf "first is: ~s~n" (cdar line))
                
                (printf "is not in function-hash ~n")
            )
        )
    )
   
    ;keep going down the thingamajig until nothing else to print
    (when (> (length line) 1 )
        (printf "in recursion~n")
        (execute-the-print (cdr line))
    )
)

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    ;checking if it is a print statement
	(for-each (lambda (line) (print-statement line)) program)

    ;this part putting into label-table
    ;check for syntax errors?
    (for-each 
        (lambda (line)  (when (>= (length line) 2) (set-label! (cdr line) (car line)) ) )
        program
    )
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               (write-program-by-line sbprogfile program)
			  
			  ))
			  
	;prints out the label-table hash to check if it works
    ;(printf "*label-table*:~n")
    ;(hash-for-each *label-table* 
    ;(lambda (key value)
    ;(printf "~s = ~s~n" key value))) 			  
);end of main

(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))

