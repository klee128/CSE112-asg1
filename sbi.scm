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

;;holds value of all variables (updated as needed)
;;if variable not found, return 0
(define *variable-table* (make-hash))
(define (set-variable! key value)(hash-set! *variable-table* key value))


;;holds all arrays defined in program
;;created with make-vector and updated with vector-set!
(define *array-table* (make-hash))
(define (set-array! key value) (hash-set! *array-table* key value))

;;holds addresses of each line
(define *label-table* (make-hash))
(define (set-label! key value) (hash-set! *label-table* key value))

;what to do w/ input if it is a ...
(define (parse-it word)
    (cond 
        ;if is string, return string
        ((string? word) 
            word
        )
        ;if number, add 0.0 to return real number
        ((number? word) 
            (+ word 0.0)
        )
        ;if pair, calculate and return the result
        ((pair? word)
            ;if +-*/
            (if (hash-has-key? *function-table* (car word))
                (cond 
                    ;if is a procedure? if doing something and not just a vector/array
                    ((procedure? (hash-ref *function-table* (car word)))
                        (if (and (number? (cadr word)) (number? (caddr word)))
                            ;if (operator number number) format
                            ((hash-ref *function-table* (car word)) (cadr word) (caddr word))
                            ;if nested operations?
                            (apply (hash-ref *function-table* (car word)) (map parse-it (cdr word)))
                        )
                    )
                )
                (printf "is not in function-hash ~n")
            )
        )
        ;else, output error message 
        (else 
            (die '("Invalid Input: " word))
        )
    )    
)

;goes to here from the write-program-line function
;basically scanning to see the thing
(define (print-statement line)          
    ;when not just line number...
	(when (and (not (null? (cdr line)))  (> (length (cdr line)) 0) ) 
        ;caadr = function name (print, let, etc)
        ;cdadr = the other stuff thats important
        ;if found in function-table, do the thing
        (when (hash-has-key? *function-table* (caadr line))
            ((hash-ref *function-table* (caadr line)) (cdadr line))
        )
	)
)
 
;how to print :)
;line = everything after 'print'
(define (execute-the-print line)
    ;base-case
    (when (> (length line) 0)
        (display (parse-it (car line)))    
    )
    ;keep going down the thingamajig until nothing else to print
    (when (> (length line) 1 )
        (execute-the-print (cdr line))
    )
    (newline)
)

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")

    ;this part putting into label-table
    (for-each 
        (lambda (line)  (when (>= (length line) 2) (set-label! (cdr line) (car line)) ) )
        program
    )

    ;go through program line-by-line and execute it
	(for-each (lambda (line) (print-statement line)) program)
)

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
        (print, execute-the-print)      
    )
)

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

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               (write-program-by-line sbprogfile program)
			  
			  ))
			  
);end of main

(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))

