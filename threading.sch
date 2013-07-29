;;;; Copyright(c) 2013 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;;
;;;;     threading.sch is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     threading.sch is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with typecase.sch.  If not, see
;;;;     <http://www.gnu.org/licenses/>.


;;; T> is a threading macro inspired by the clojure -> and ->> macros. These
;;; macros have the form: ((or -> ->>) x form ...). The value of x is spliced
;;; in as the first or last argument, respectively, of form with its result
;;; being likewise spliced into the next form and so on. Instead of replicating
;;; these macros exactly in bigloo, I decided to create a generalized version 
;;; where _ is used to indicate where values are to be spliced in. T> has the
;;; form:
;;; (T> x form1 ...) In form1 the _ character is used to indicate where x
;;; should be spliced.
;;; examples:
;;; (T> 4 (+ _ 3) (* 3 _)) => 21
;;; (T> '( 1 2 3 4) (map odd? _) (map (lambda (x) (* x 2)) _)) => (2 6) 
(define-expander T>
   (define (substitute-mark form v)
      (map (lambda (x) (if (pair? x)
			   (substitute-mark x v)
			   (if (eq? x '_)
			       v x))) form))
   (lambda (x e)
      (match-case x
	 ((T> ?source ?only-form)
	  (let ((subst-sym (gensym '_)))
	     (e `(let ((,subst-sym ,source))
		    ,(substitute-mark only-form subst-sym)) e)))
	 ((T> ?source ?first-form . ?rest)
	  (let ((subst-sym (gensym '_)))
	     (e `(T> (let ((,subst-sym ,source))
			,(substitute-mark first-form subst-sym))
		    ,@rest) e)))
	 (else
	  (error "T>" "invalid form" x)))))
	 