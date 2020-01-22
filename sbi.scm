#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr

;; Kelsy Lee, klee128
;; Sruthi Jaganathan, sjaganat
;; CSE112 Assignment 1

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
        ;if key in *variable-table*, return value
        ((hash-has-key? *variable-table* word)
            (hash-ref *variable-table* word)
        )
        ;if key in *array-table* return the list

        ;if pair, calculate and return the result
        ((pair? word)
            ;if is in function-table...
            (when (hash-has-key? *function-table* (car word))
                (cond 
                    ;if is a procedure? if doing something and not just a vector/array
                    ((procedure? (hash-ref *function-table* (car word)))
                        (apply (hash-ref *function-table* (car word)) (map parse-it (cdr word)))
                    )
                    ;if is an array, do it here
                )
            )
        )
        ;else, output error message 
        (else 
            (die '("Invalid Input: " word))
        )
    )    
)

;goes to here from the write-program-line function
;going through the program line by line
(define (print-statement program line_number)
    ;(printf "in print-state. on index ~s~n" line_number)
    (if (< line_number (length program))
        (    ;(define line (list-ref program line_number))
            (if (and (not (null? (cdr (list-ref program line_number))))  (> (length (cdr (list-ref program line_number))) 0) )
                (if (pair? (cadr (list-ref program line_number)))
                    ;2nd thing is function. do function
                    (when (hash-has-key? *function-table* (caadr (list-ref program line_number)))
                        (cond 
                            ((eqv? (caadr (list-ref program line_number)) 'if)
                                (execute-the-if program (cdadr (list-ref program line_number)))
                            )
                            ((eqv? (caadr (list-ref program line_number)) 'goto)
                                ;(printf "go to execute. label is ~s~n" (cadadr (list-ref program line_number)))
                                (execute-the-goto program (cdadr (list-ref program line_number)))
                            )
                            (else 
                                ((hash-ref *function-table* (caadr (list-ref program line_number))) (cdadr (list-ref program line_number)))
                            )
                        )
                    )
                    ;2nd thing is label 
                    (if (null? (cddr (list-ref program line_number)))
                        ;only has label
                        (print-statement program (+ line_number 1))
                        ;has function to solve
                        (when (hash-has-key? *function-table* (caaddr (list-ref program line_number)))
                            (cond 
                                ( (eqv? (caaddr (list-ref program line_number)) 'if)
                                    (execute-the-if program (cdaddr (list-ref program line_number))))
                                (else
                                    ((hash-ref *function-table* (caaddr (list-ref program line_number))) (cdaddr (list-ref program line_number)))
                                )
                            )
                        )
                    );end of second thing is label
                )
                (print-statement program (+ line_number 1))
            )
            ;move on to next line
            (print-statement program (+ line_number 1))
        )
        (exit)
    )
)          

 
;how to print :)
;line = everything after 'print'
(define (execute-the-print line)
    ;base-case
    (when (> (length line) 0)
        (display (parse-it (car line)))    
    )
    ;recursively print rest of line
    (when (> (length line) 1 )
        (execute-the-print (cdr line))
    )
    (newline) 
)

;line = variable_name + value
;array vs variable? check later!!!!!!
(define (execute-the-let line)
    (set-variable! (car line) (parse-it (cadr line)))
)

;line = asub + name + length
(define (execute-the-dim line)
    (set-array! (cadar line) (make-vector (caddar line) 0.0) )  
)

;evaluate and check the condition, if it is an if statement
(define (execute-the-if program line)
    (when (> (length line) 0)
        (when (parse-it(car line))
            ;Check if the label table has this label, if it does, extract the line number associated with it, 
            ;transfer control to this line
            (when (hash-has-key? *label-table* (car(cdr line)))
                    ;(display "Going to enter print-statement again")
                    (print-statement program (hash-ref *label-table* (car(cdr line))))
            )
        )
    )
)

(define (execute-the-goto program line)
    ;(printf "in execute. line is ~s~n" (car line))
    (when (hash-has-key? *label-table* (car line))
        ;(printf "in execute. go to index ~s~n" (hash-ref *label-table* (car line)))
        (print-statement program (hash-ref *label-table* (car line)))
    )
)

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")

    ;this part putting into label-table (do line-number -1 to get right index)
    (for-each 
        (lambda (line)  
            (when (> (length line) 1) 
                (when (not (list? (cadr line)))
                    (set-label! (cadr line) (- (car line) 1))
                )
            ) )
        program
    )


    ;go through program line-by-line and execute it
    (print-statement program 0)


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
        (<,<) (>,>) (<=,<=) (>=,>=)
        (^,expt) 
        (print, execute-the-print) ;need to do array
        (let, execute-the-let)     ;array vs variable? + replace? something about asub?
        (dim, execute-the-dim)     ;not used much so should be good lol
        (if, execute-the-if)  
        (goto, execute-the-goto)
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

    ;(printf "*label-table*:~n")
    ;(hash-for-each *label-table* (lambda (key value) (printf "~s = ~s~n" key value)))
			  

			  
);end of main

(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))

