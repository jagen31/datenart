#lang racket

(require art 2htdp/image (for-syntax syntax/parse racket/syntax))

(define-coordinate (entity [name]))
(define-art-object (attribute [id]))

(define-art-object (schema [type args]))

(define-coordinate (table []))

(define-hom-merge-rule table
  (λ (l r _ __ ___)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      (error 'merge-table "oops, cannot merge table for now"))))
  
(define-hom-within?-rule table (λ (l r _ __ ___)
  (syntax-parse #`(#,l #,r)
    [(({~literal table} ln) ({~literal table} rn))
     (eq? (syntax-e #'ln) (syntax-e #'rn))])))

(define-for-syntax (expr-table-name stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'table) 
    [(_ name) #'name]))
 

(define-coordinate (column []))

(define-art-object (column-names []))
(define-art-object (column-mapping []))

(define-art-object (insert [cols]))

(define-mapping-rewriter (run-insert [(: ins insert)])
  (λ (stx ins)
    (syntax-parse ins
      [(_ val ...)
       #:with (_ col ...) (require-context (lookup-ctxt) stx #'column-names)
       (qq-art stx (row (@ [(column col)] val) ...))])))

(define-art-embedding (row [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))


(define-drawer draw-table
  (λ (stx) (syntax-parse stx [({~datum table} name) #'(text (format "Table: ~a" 'name) 24 'blue)])))

(define-drawer draw-column-names
  (λ (stx)
    (syntax-parse stx
      [(_ col ...)
       #:with table-name (expr-table-name stx)
       #'(above/align 'left
           (text (format "Column names for ~a:" (symbol->string 'table-name)) 24 'blue)
           (text (string-join (map symbol->string (list 'col ...)) " | ") 24 'blue))])))

(define-drawer draw-row
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       #:with table-name (expr-table-name stx)
       (define len (length (syntax->list #'(expr ...))))
       (define row-drawings
         (map (λ (e) (parameterize ([drawer-width (/ (drawer-width) len)]) (drawer-recur e))) (syntax->list #'(expr ...))))
       (define row-drawings* #`(beside #,@row-drawings))
    
       #`(above/align 'left
           (text (format "Row for ~a:" (symbol->string 'table-name)) 24 'blue)
           #,row-drawings*)])))

(register-drawer! table draw-table)
(register-drawer! column-names draw-column-names)
(register-drawer! row draw-row)



(define-art the-table
  (@ [(table sales)]
    (column-names id customer-first-name customer-last-name customer-email product-name date)))

(define-art the-table-with-data
  the-table
  (@ [(table sales)]
    (insert (number 1) (string "Joe") (string "Schmoe") (string "joe@schmoe.com") (string "Gloves") (string "03/27"))
    (run-insert)))

(qr the-table-with-data)
(realize (draw-trace-realizer [800 200]) the-table-with-data)

(dr the-table-with-data)

(define-art the-model 
  (@ [(entity customer)] (attribute name) (attribute email))
  (@ [(entity product)] (attribute name))
  (@ [(entity sale)] (attribute customer) (attribute product) (attribute date)))

  (define-art the-table-with-model+mapping
    the-model the-table-with-data
    (@ [(table sales)])
      (column-mapping 
        [customer [customer-first-name . name] [customer-last-name . name] [customer-email . email]]
        [product [product-name . name]]
        [sale [date . date]]))

(dr the-table-with-model+mapping)

(define-mapping-rewriter (3nf [(: cols column-mapping)])
  (λ (stx colmap)
    (syntax-parse colmap
      [(_ [e-name [col-name . attr-name] ...] ...)
       (define lookup (syntax->datum #'((e-name [col-name . attr-name] ...) ...)))
       (define table-name (context-ref (get-id-ctxt colmap) #'table))
       (define table-cols (require-context (lookup-ctxt) colmap #'column-names))
       (define/syntax-parse [(new-table-name [new-table-col new-table-mapping] ...) ...]
         (for/list ([entity lookup])
           (define table-name* (format-id #f "~a-~a" table-name (car entity)))
           (cons table-name* (cdr entity))))
       (qq-art colmap 
         (context (@ [(table new-table-name)] (column-names new-table-col ...)) ...))])))
