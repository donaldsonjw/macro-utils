;;;; Copyright(c) 2013 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;;
;;;;     typecase.sch is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     typecase.sch is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with typecase.sch.  If not, see
;;;;     <http://www.gnu.org/licenses/>.



;;; typecase has the following form:
;;; (typecase <form> (((or (<type symbol> ...) else) . <body>) ...))
;;; The type of <form> is matched against the symbol list of each clause.
;;; If a clause is applicable the clauses <body> is executed and the typecase
;;; is exited. As with a regular case statement, a default clause can be
;;; specified with an else clause.
;;; ex. (typecase 5 ((bint) "bint") (else "not bint")) => "bint"
;;; ex. (typecase 's ((bint) "bint") (else "not bint")) => "not bint"
(define-expander typecase
   (define (expand-tc-clause x e)
      (match-case x
	 (((and ?symbols ((? symbol?) (? symbol?) ...)) . ?body)
	  (e `(,symbols  ,@body) e))
	 (((kwote else)  . ?body)
	  (e `(else  ,@body) e))
	 (else
	  (error "typecase" "invalid typecase clause" x))))
   
   (define (expand-tc-clauses cs e)
      (let loop ((lst cs)
		 (res '()))
	 (if (pair? lst)
	     (loop (cdr lst)
	        (cons (expand-tc-clause (car lst) e)
		   res))
	     (reverse res))))
   (lambda (x e)
      (match-case x
	 ((?- ?form  .  (and ?tc-clauses (((or (kwote else) ((? symbol?) (? symbol?) ...)) . ?-) ...)))
	  (e `(case (string->symbol (typeof ,form)) ,@(expand-tc-clauses tc-clauses e)) e))
	 (else
	  (error "typecase" "invalid form" x)))))



