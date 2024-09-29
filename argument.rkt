#lang racket/base
(require (for-template syntax/parse/define))
(provide (struct-out argument-info)
         (struct-out arguments-info)
         parse-arguments)

(struct argument-info (keyword name value) #:constructor-name make-info)
(struct arguments-info (fixed rest) #:constructor-name make-info-list)

(define (make-info-list/check-names fixed rest)
  (cond ((check-duplicate-identifier `(,@(map argument-info-name fixed) ,@(if rest (list rest) null)))
         => (lambda (id) (raise-syntax-error #f "duplicate identifiers" id)))
        (else (make-info-list fixed rest))))

(define-splicing-syntax-class argument
  #:description "argument syntax of lambda/lru-cache"
  (pattern (~seq name:id))
  (pattern (~seq name:id value:expr))
  (pattern (~seq keyword:keyword name:id))
  (pattern (~seq keyword:keyword (name:id value:expr))))
(define (parse-argument stx)
  (syntax-parse stx
    ((name:id)
     (make-info #f #'name #f))
    (((name:id value:expr))
     (make-info #f #'name #'value))
    ((keyword:keyword name:id)
     (make-info #'keyword #'name #f))
    ((keyword:keyword (name:id value:expr))
     (make-info #'keyword #'name #'value))
    (_ (raise-syntax-error #f "fail to parse the argument" stx))))

(define (parse-arguments stx)
  (syntax-parse stx
    (arg:id (make-info-list/check-names null #'arg))
    ((arg:argument ...)
     (make-info-list/check-names
      (map parse-argument (syntax->list #'(arg ...)))
      #f))
    ((arg:argument ... . rest:id)
     (make-info-list/check-names
      (map parse-argument (syntax->list #'(arg ...)))
      #'rest))
    (_ (raise-syntax-error #f "fail to parse arguments" stx))))
