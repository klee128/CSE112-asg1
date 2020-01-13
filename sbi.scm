#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; always have ^^ or it won't be evaluated
;; $Id: sbi.scm,v 1.11 2019-12-11 16:16:51-08 - - $
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

;; define the standard input/output/error
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

;;file input --> linked list?
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;;the "teminal-port *stdin* = #t" part i think
(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;;holds all functions (including operators)
; CAN'T BE CHANGD BY USER
(define *function-table* (make-hash))
;sets up the function that insersts things into *function-table*
(define (set-function! key value)
    (hash-set! *function-table* key value)
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
;;initialized by scanning list w/ read at beginning
(define *label-table* (make-hash))
(define (set-label! key value)
    (hash-set! *label-table* key value)
)


;;======
;; sbi.scm "filename"
;;======
;;then each line in the input ifle
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;----------------------------------------------------------------
; main function (what u do here will show up)
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program)))

;prints out the function-table hash to check if it works
;     (printf "*function-table*:~n")
;    (hash-for-each *function-table* 
;        (lambda (key value)
;                (printf "~s = ~s~n" key value))) 
) ;end of main

(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))

