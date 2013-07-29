;;;; Copyright(c) 2013 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;;
;;;;     pset.sch is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     pset.sch is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with pset.sch.  If not, see
;;;;     <http://www.gnu.org/licenses/>.



;;; pset! has the following form
;;; (pset! <var0> <val0> ... <varn> <valn>) 
;;; pset! is very similar to set!, except that the assignments happen in
;;; parallel. First all the forms are evaluated, and then the variables are set.
;;; For example, (define a 5) (define b 4) (pset! a b b a) => a = 4 and b = 5
;;; It also works with the -> special form.
;;; (define-class animal legs)
;;; (let ((spider::animal (instantiate::animal (legs 8)))
;;;          (b 5))
;;;     (pset! (-> spider legs) b b (-> spider legs) a) =>
;;;     (-> spider legs) = 5 and b = 8
(define-expander pset!
   (define (pset-form-valid? body)
      (let loop ((lst body))
	 (if (pair? lst)
	     (match-case lst
		(((or (? symbol?)
		      ((kwote ->) (? symbol?) (? symbol?) ...))
		  ?form ???-)
		 (loop (cddr lst)))
		(else
		 #f))
	     #t)))
   (define (gen-temps-alist body)
      (let loop ((lst body)
		 (res '()))
	 (if (pair? lst)
	     (let ((gs (gensym)))
		(loop (cddr lst)
		   (cons (list (car lst) gs (cadr lst)) res)))
	     (reverse res))))
   (define (gen-lclauses tl)
      (let loop ((lst tl)
		 (res '()))
	 (if (pair? lst)
	     (let ((t (car lst)))
		(loop (cdr lst)
		   (cons (list (cadr t) (caddr t)) res)))
	     (reverse res))))
   (define (gen-lbody tl)
      (let loop ((lst tl)
		 (res '()))
	 (if (pair? lst)
	     (let ((t (car lst)))
		(loop (cdr lst)
		   (cons (list 'set! (car t) (cadr t)) res)))
	     (reverse res))))
   (define (gen-pset tl)
      `(let ,(gen-lclauses tl) ,@(gen-lbody tl)))
   (lambda (x e)
      (when (not (pset-form-valid? (cdr x)))
	 (error "pset!" "invalid form" x))
      (let ((tl (gen-temps-alist (cdr x))))
	 (e (gen-pset tl) e)))) 
