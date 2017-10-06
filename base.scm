(module base (export
	      alist-change just-pair?
	      boolean-and list-of-lists? call-on-all-sublists
	      checked-list->vector recursive-list->vector)
  (reexport chicken scheme srfi-1)
  ;; Interning as many symbols as possible by adding i suffix
  (define (alist-change-i al k v)
    (map (lambda (p) (if (equal? (car p) k)
		    (cons k v)
		    p))
	 al))

  (define (alist-change al k v) (alist-change-i al k v))
  
  (define (just-pair?-i p)
    (if (pair? p)
	(if (pair? (cdr p))
	    #f
	    #t)
	#f))

  (define (just-pair? p) (just-pair?-i p))

  (define (boolean-and-i list)
    (if (boolean? list)
	list
	(let ((flist (filter (lambda (l) (not l)) list)))
	  (if (equal? flist '())
	      #t
	      (car flist)))))

  (define (boolean-and list) (boolean-and-i list))
  
  (define (list-of-lists?-i list)
    (if (pair? list)
	(not (boolean-and (map (lambda (l) (not (list? l))) list)))
	#f))

  (define (list-of-lists? list) (list-of-lists?-i list))

  (define (call-on-all-sublists-i f l)
    (let ((partial-c-o-a-s (cut call-on-all-sublists f <>)))
      (cond
       ((list-of-lists? l)
	(f (map partial-c-o-a-s l)))
       ((null? l)
	l)
       ((not-pair? l)
	l)
       (else
	(f l)))))

  (define (call-on-all-sublists f l) (call-on-all-sublists-i f l))

  (define (checked-list->vector-i l)
    (if (list? l)
	(list->vector l)
	l))

  (define (checked-list->vector l) (checked-list->vector-i l))

  (define (recursive-list->vector-i l)
    (call-on-all-sublists checked-list->vector l))

  (define (recursive-list->vector l) (recursive-list->vector-i l)))
