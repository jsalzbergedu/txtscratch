;; -*- geiser-scheme-implementation: chicken -*-
;;; Commentary:
;; The goal of this is to take in a scratch json file,
;; and accept sexp's that will generate proper functions
;; in scratch, and hopefully allow more composability of types

;; Getting the program arguments:
;(assert (= (length (argv)) 2))
(use chicken)
(use medea)
(use utils)

;; Getting the original JSON structure, then switching arrays to lists
(define (get-json-infile file-path)
  (read-json (read-all file-path)))

;(define infile (get-json-infile (car (cdr (argv)))))
;(set! infile (get-json-infile "/home/jacob/FEDD/source/project.json")); for testing purposes
(define infile-vec (get-json-infile "/home/jacob/FEDD/source/project.json"))

(define array-as-list-parser (cons 'array (lambda (x) x)))
(json-parsers (cons array-as-list-parser (json-parsers)))

(define infile-alist (get-json-infile "/home/jacob/FEDD/source/project.json"))

(define array-as-list-parser (cons 'array (lambda (x) x)))
(json-parsers (cons array-as-list-parser (json-parsers)))

;; Deep map utility function
(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
	  ((pair? x) (map deep x))
	  (else (f x)))))

;; If it is a list containing lists, map on the contained lists
;; else if it is just a list, apply f to it
;; else if it is not a list or a list of lists, just return it itself
(define (boolean-and list)
  (if (boolean? list)
      list
      (let ((flist (filter (lambda (l) (not (identity l))) list)))
	(if (equal? flist '())
	    #t
	    (car flist)))))

(define (list-of-lists? list)
  (if (pair? list)
      (not (boolean-and (map (lambda (l) (not (list? l))) list)))
      #f))

(define (call-on-all-sublists f l)
  (let ((partial-c-o-a-s (cut call-on-all-sublists f <>)))
    (cond
     ((list-of-lists? l)
      (f (map partial-c-o-a-s l)))
     ((null? l)
      l)
     ((atom? l)
      l)
     (else
      (f l)))))

(define (checked-list->vector l)
  (if (list? l)
      (list->vector l)
      l))

(define (recursive-list->vector l)
  (call-on-all-sublists checked-list->vector l))

;; Scratch data structure
(define (children scratchobj) (cadr (assoc 'children scratchobj)))

(define (script x y . rest)
  (list x y rest))

(define (block f . args)
  (append (list f) args))

(define (finalize-script s)
  (recursive-list->vector s))

;; Just a test
;; Adding functions will have the syntax (defun asdf f)
;; Where f takes two args and passes then down to get added to script and info
(define (change-scriptcount scratchproj n)
  (cons 'info (let ((info (cdr (assoc 'info scratchproj))))
		(map (lambda (p) (if (string= (symbol->string (car p)) "scriptCount")
				(cons (car p) (+ n (cdr p)))
				p))
		     info))))
		    
(define (append-script scratchproj script)
  (let ((scripts (assoc 'scripts (children scratchproj))))
    (cons 'scripts
	   (recursive-list->vector
	    (cdr
	     (append scripts (list script)))))))

(define (add-block-test scratchproj)
  (list (append-script scratchproj (script 10 10 (block "whenGreenFlag") (block "forward: " 10)))
	(change-scriptcount scratchproj 1)))
