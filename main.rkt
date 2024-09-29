#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require (for-syntax "argument.rkt" racket/base) syntax/parse/define racket/vector)
(provide lambda/lru-cache)

(define-syntax-parser lambda/lru-cache
  ((_ cnt:exact-positive-integer args body ...+)
   (let* ((formals-info (parse-formals #'args))
          (fixed (formals-info-fixed formals-info))
          (positional (filter (lambda (i) (and (not (argument-info-keyword i)) (not (argument-info-value i)))) fixed))
          (positional/optional (filter (lambda (i) (and (not (argument-info-keyword i)) (argument-info-value i))) fixed))
          (keyword (filter (lambda (i) (and (argument-info-keyword i) (not (argument-info-value i)))) fixed))
          (keyword/optional (filter (lambda (i) (and (argument-info-keyword i) (argument-info-value i))) fixed))
          (rest (formals-info-rest formals-info)))
     #`(let* ((vec (make-vector cnt #f)) ;; real records are all lists
              (tbl (make-hash))

              (update-records!
               (lambda (record result)
                 (cond ((hash-has-key? tbl record)
                        (define loc (vector-member record vec))
                        (vector-copy! vec loc vec (add1 loc) cnt))
                       (else (hash-remove! tbl (vector-ref vec 0))
                             (vector-copy! vec 0 vec 1 cnt)
                             (hash-set! tbl record result)))
                 (vector-set! vec (sub1 cnt) record))))
         (lambda args
           (let* ((record
                   (vector-immutable
                    #,@(map argument-info-name positional)
                    #,@(map argument-info-name positional/optional)
                    #,@(map argument-info-name keyword)
                    #,@(map argument-info-name keyword/optional)
                    #,@(if rest (list rest) '())))
                  (result (hash-ref tbl record (lambda () body ...))))
             (update-records! record result)
             result))))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (define (fib n)
    (if (zero? n)
        1
        (if (= n 1)
            1
            (+ (fib (sub1 n))
               (fib (- n 2))))))
  (define fib/cached
    (lambda/lru-cache
     5
     (n)
     (if (zero? n)
         1
         (if (= n 1)
             1
             (+ (fib/cached (sub1 n))
                (fib/cached (- n 2)))))))

  (define rand-num (random 0 20))
  (check-equal? (fib rand-num) (fib/cached rand-num))
  (check-equal? ((lambda/lru-cache 1 a (apply + a)) 1 2 3) 6)
  (check-equal? ((lambda/lru-cache 1 (a . b) (apply + a b)) 1 2 3) 6)
  (check-equal? ((lambda/lru-cache 1 (#:a a) a) #:a 1) 1)
  (let* ((a 1) (proc (lambda/lru-cache 2 (b) (dynamic-wind void (lambda () a) (lambda () (set! a b))))))
    (check-equal? (proc 1) 1)
    (check-equal? (proc 2) 1)
    (check-equal? (proc 1) 1)
    (check-equal? (proc 2) 1)
    (check-equal? (proc 3) 2))

  ;; Benchmark
  (writeln '(time (fib 40)))
  (void (time (fib 40)))
  (writeln '(time (fib/cached 40)))
  (void (time (fib/cached 40)))
  (writeln '(time (fib/cached 40000)))
  (void (time (fib/cached 40000))))
