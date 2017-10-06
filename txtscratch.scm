;; -*- geiser-scheme-implementation: chicken -*-
;;; Commentary:
;; The goal of this is to take in a scratch json file,
;; and accept sexp's that will generate proper functions
;; in scratch, and hopefully allow more composability of types

;; Getting the program arguments:
;(assert (= (length (argv)) 2))
(use base)
(use medea)
(use utils)
(use alist-lib)
(use vector-lib)

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

;; Scratch data structure --- writing
(define (children scratchobj) (cadr (assoc 'children scratchobj)))

(define (children-json scratchobj) (assoc 'children scratchobj))

(define (script x y . rest)
  (list x y rest))

(define (block f . args)
  (append (list f) args))

;; Scratch data structure --- reading
(define (get-sprite children-json sprite)
  (let g-s ((children (vector->list (cdr children-json))))
    (if (pair? children)
	(if (string= (cdaar children) sprite)
	    (car children)
	    (g-s (cdr children)))
	#f)))

;; Scratch data structure --- zipping 	
(define (replace-sprite children-json sprite-name sprite)
  (cons 'children
	(list->vector
	 (let r-s ((children (vector->list (cdr children-json))))
	   (if (pair? children)
	       (if (string= (cdaar children) sprite-name)
		   (cons sprite (r-s (cdr children)))
		   (cons children (r-s (cdr children))))
	       '())))))

(define (append-script scripts script)
    (cons 'scripts
	   (recursive-list->vector
	    (cdr
	     (append scripts (list script))))))

 (define (add-script-to-sprite sprite appended-script)
   (alist-change sprite 'scripts (cdr appended-script)))

;; Adding functions will have the syntax (defun asdf f)
;; Where f takes two args and passes then down to get added to script and info
;; Working with the toplevel json file:
(define (change-scriptcount scratchproj n)
  (cons 'info (let* ((info (cdr (assoc 'info scratchproj)))
		     (script-count (cdr (assoc 'scriptCount info))))
		(alist-change (cdr info) 'scriptCount (+ 1 script-count)))))
		    

;; (define (add-block-test scratchproj) (list (append-script scratchproj (script 10 10 (block "whenGreenFlag") (block "forward: " 10)))
;; 	(change-scriptcount scratchproj 1)))

;; (define (add-block scratchproj-alist scratchproj-json script)
;;   (let ((appended-script (cdr (append-script scratchproj-alist script)))
;; 	(changed-scriptcount (change-scriptcount scratchproj-alist 1)))
;;     (alist-change (alist-change scratchproj-json
;; 		  'children
;; 		  (list->vector (list (add-script-to-children scratchproj-alist appended-script))))
;; 		  'info changed-scriptcount)))
