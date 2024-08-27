#lang racket

(require (except-in art bitmap) 2htdp/image (for-syntax syntax/parse racket/syntax racket/list racket/string))

(define-coordinate (entity [name]))
(define-art-object (attribute [id]))
(define-art-object (has [id]))

(define-art-object (schema [type args]))

(define-coordinate (table []))

(define-hom-merge-rule table
  (λ (l r _ __ ___)
    (let/ec break
      (unless l (break r))
      (unless r (break l))
      l
      #;(error 'merge-table "oops, cannot merge table for now"))))
  
(define-hom-within?-rule table (λ (l r _ __ ___)
  (syntax-parse #`(#,l #,r)
    [(({~literal table} ln) ({~literal table} rn))
     (eq? (syntax-e #'ln) (syntax-e #'rn))])))

(define-for-syntax (expr-table-name stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'table) 
    [(_ name) #'name]))
 

(define-coordinate (column []))

(define-art-object (column-names []))
(define-art-object (column-art-types []))
(define-art-object (column-sql-types []))

(define-art-object (primary-key [col]))

(define-art-object (column-mapping []))

(define-art-object (create [expr]))
(define-art-object (insert [cols]))
(define-art-object (select []))

(define-mapping-rewriter (run-insert [(: ins insert)])
  (λ (stx ins)
    (syntax-parse ins
      [(_ val ...)
       #:with (_ col ...) (require-context (lookup-ctxt) stx #'column-names)
       (qq-art stx (row (@ [(column col)] val) ...))])))

(define-mapping-rewriter (run-select [(: sel select)])
  (λ (stx sel)
    (syntax-parse sel
      [(_ [col ...] {~seq #:where where-expr} {~seq #:into into-expr:id})
       #:with (_ col* ...) (require-context (lookup-ctxt) sel #'column-names)
       (define cols* (syntax->datum #'(col* ...)))
       (define ixs (for/list ([col (syntax->datum #'(col ...))]) (index-of cols* col)))
       (define rows (context-ref*/within (lookup-ctxt) (get-id-ctxt sel) #'row))

       (define rows*
         (for/list ([row rows])
           (define/syntax-parse (_ row-data- ...) row)
           (syntax->list #'(row-data- ...))))
       
       (define filtered-rows
         (filter (λ (row-data) 
           (define lookup
             (for/list ([item row-data] [col cols*])
               (car (rewrite #`(name@ #,col #,item)))))
           (define result
             (run-art-exprs
               (list #'where-expr)
               '()
               ;; rewrite with the selected columns bound to names. reference with `ref*`
               lookup))
           (unless (= (length result) 1) (raise-syntax-error 'run-select "oops" #'where-expr))
           (syntax-parse (car result)
             [({~literal boolean} val:boolean) (syntax-e #'val)]
             [_ (raise-syntax-error 'run-select "oops" #'where-expr)]))
          rows*))

      (define selected-rows 
        (for/list ([row filtered-rows]) (map (λ (ix) (list-ref row ix)) ixs)))

       #`(context 
         #,(qq-art (remove-from-id-ctxt sel #'table)
           (@ [(table into-expr)]
             (column-names col ...)
             #,@(for/list ([row-data selected-rows]) 
                  ;; FIXME jagen preserve row source location
                  #`(row #,@row-data)))))])))

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
           (text (string-join (map symbol->string (list 'col ...)) " | ") 24 'goldenrod))])))

(define-drawer draw-row
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       #:with table-name (expr-table-name stx)
       (define len (length (syntax->list #'(expr ...))))
       (define row-drawings
         (map (λ (e) (parameterize ([drawer-width (/ (drawer-width) len)]) (drawer-recur e))) (syntax->list #'(expr ...))))
       (define row-drawings* #`(beside #,@row-drawings empty-image))
    
       #`(above/align 'left
           (text (format "Row for ~a:" (symbol->string 'table-name)) 24 'blue)
           #,row-drawings*
           empty-image)])))

(register-drawer! table draw-table)
(register-drawer! column-names draw-column-names)
(register-drawer! row draw-row)



#;(define-art the-table
  (@ [(table sales)]
    (column-names id customer-first-name customer-last-name customer-email associate-name product-name date)
    (column-sql-types int (char 32) (char 32) (char 128) (char 32) (char 32) timestamp)
    (primary-key id)))

#;(define-art the-table-with-data
  the-table
  (@ [(table sales)]
    (insert (number 1) (string "Joe") (string "Schmoe")
            (string "joe@schmoe.com") (string "Gloves") (string "Patrick") (string "03/27"))
    (run-insert)))

#;(realize (draw-trace-realizer [800 200]) the-table-with-data)

#;(dr the-table-with-data)

#;(define-art the-model 
  (@ [(entity customer)] (has associate) (attribute name) (attribute email))
  (@ [(entity product)] (attribute name))
  (@ [(entity associate)] (has customer) (attribute name))
  (@ [(entity sale)] (has customer) (has product) (attribute date)))

#;(define-art the-table-with-model+mapping
  the-model the-table-with-data
  (@ [(table sales)]
     (column-mapping
      [customer [customer-first-name . name] [customer-last-name . name] [customer-email . email]]
      [product [product-name . name]]
      [sale [date . date]])))

#;(qr the-table-with-model+mapping)

(define-mapping-rewriter (3nf [(: cols column-mapping)])
  (λ (stx colmap)
    (syntax-parse colmap
      [(_ [e-name [col-name . attr-name] ...] ...)
       #:do [(define lookup (syntax->datum #'((e-name [col-name . attr-name] ...) ...)))]
       #:with (_ table-name) (context-ref (get-id-ctxt colmap) #'table)
       #:with {~and cn-expr (_ table-cols ...)} (require-context (lookup-ctxt) colmap #'column-names)
       #:with ({~and r-expr (_ row-value ...)} ...) (context-ref*/within (lookup-ctxt) (get-id-ctxt colmap) #'row)

       (define table-cols* (syntax->datum #'(table-cols ...)))

       ;; create a new table for each entity which has attributes in this table
       (define/syntax-parse [(new-table-name (new-entity [new-table-col . new-table-mapping] ...)) ...]
         (for/list ([entity lookup])
           (define table-name* (format-id #f "~a-~a" (syntax-e #'table-name) (car entity)))
           (cons table-name* (list entity))))

       ;; split the rows up over the new tables
       (define/syntax-parse [(new-table-name2 [new-row-value ...]) ...]
         (map car
           (for/list ([table-name+cols (syntax->datum #'((new-table-name new-table-col ...) ...))])
             (for/list ([row (map syntax->list (syntax->list #'((row-value ...) ...)))] [id (in-naturals)])
               (define row*
                 (for/list ([col (cdr table-name+cols)])
                   (define ix (index-of table-cols* col))
                   (list-ref row ix)))
               (list (car table-name+cols) (cons #`(number #,(add1 id)) row*))))))
       (qq-art (remove-from-id-ctxt colmap #'table)
         (context
          #,(delete-expr #'cn-expr)
          #,@(map delete-expr (syntax->list #'(r-expr ...)))
          (@ [(table new-table-name)]
             (column-names id new-table-col ...)
             (column-mapping (new-entity [new-table-col . new-table-mapping] ...))) ...

          (@ [(table new-table-name2)]
             (row new-row-value ...)) ...))])))

#;(displayln "WIDE TABLE:::::")
#;(dr the-table-with-model+mapping
    (delete column-mapping) (delete attribute))

#;(displayln "3NF:::::")
#;(dr the-table-with-model+mapping
  (3nf) (delete column-mapping) (delete attribute))


(define-art-rewriter table-header->ddl
  (λ (stx)
    (define/syntax-parse {~and cn-expr (_ name ...)} (require-context (lookup-ctxt) stx #'column-names))
    (define/syntax-parse (_ type ...) (require-context (lookup-ctxt) stx #'column-sql-types))
    
    (qq-art #'cn-expr (create table [name type] ...))))

(define-art-realizer postgres-realizer
  (λ (stx)
    #`#,(string-join
      (for/list ([expr (current-ctxt)])
        (syntax-parse expr
          [({~literal create} {~datum table} [name type] ...)
           (define col-defs
             (string-join
               (for/list ([name (syntax->list #'(name ...))] [type (syntax->datum #'(type ...))])
                 (define type*
                   (syntax-parse type
                     [({~datum char} n) (format "CHAR(~s)" (syntax-e #'n))]
                     [{~datum int} "BIGINT"]
                     [{~datum timestamp} "TIMESTAMP"]))
                 (format "~a ~a" (string-replace (symbol->string (syntax-e name)) "-" "_") type*)) ",\n"))
           (define stmt-str
             (string-append 
             """
             CREATE TABLE ~a
             (
               ~a
             );
             """)
           )
           (format stmt-str (syntax-e (expr-table-name expr)) col-defs)]
          [_ ""]))
      "\n\n")))

#;(match-define (list out in _ err _)
  (process 
    (format "psql \"user=jqpublic password=password\" -c \"~a\"" 
            (realize (postgres-realizer) the-table (@ [(table sales)] (table-header->ddl))))))

#;(displayln (port->string out))

#;(qr the-table-with-data
    (@ [(table sales)] (select [customer-first-name] #:where (boolean #t) #:into test))
    (run-select))

(provide (all-defined-out) (for-syntax (all-defined-out)))
