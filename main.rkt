#lang racket

(require art)



(define-art core
  
  (concepts product store sale date customer promotion)

  ;; sql-ish representations of the concepts
  (name@ sql
    (@ [(concept product)] (attributes sku description brand category))
    (@ [(concept store)] (attributes state city))
    (@ [(concept sale)]
       (attribute date product store promotion customer quantity net-price discount-price))
    (@ [(concept date)] (attribute year month day weekday is-holiday))
    
    (name@ general-sql-types
      (sql-types))))

(define-art sale-service
  core
  (ref* sql general-sql-types)
  (star-schema sale))

(realize (mysql-ddl-realizer)
  sale-service
  (sql-types->mysql-types)
  (star-schema->mysql-tables))

