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

(require (for-syntax "argument.rkt" racket/base racket/fixnum) "bd-list.rkt" syntax/parse/define racket/unsafe/ops)
(provide lambda/lru-cache)

(define none (gensym))

(define-syntax-parser lambda/lru-cache
  ((_ cnt:exact-positive-integer args body ...+)
   (let* ((formals-info (parse-formals #'args))
          (fixed (formals-info-fixed formals-info))
          (positional (filter (lambda (i) (and (not (argument-info-keyword i)) (not (argument-info-value i)))) fixed))
          (positional/optional (filter (lambda (i) (and (not (argument-info-keyword i)) (argument-info-value i))) fixed))
          (keyword (filter (lambda (i) (and (argument-info-keyword i) (not (argument-info-value i)))) fixed))
          (keyword/optional (filter (lambda (i) (and (argument-info-keyword i) (argument-info-value i))) fixed))
          (rest (formals-info-rest formals-info))

          (fx? (fixnum-for-every-system? (syntax->datum #'cnt)))
          (n:< (if fx? #'unsafe-fx< #'<))
          (n:+ (if fx? #'unsafe-fx+ #'+))
          )
     #`(let* ((init-node (make-initial-node))

              (tail-box (box init-node))
              (head-box (box init-node))
              (tbl (make-hash))
              (node-cache (make-hash))

              (len-box (box 0))

              (insert-record-to-tail!
               (lambda (record)
                 (define tail (unsafe-unbox* tail-box))
                 (define n (insert-node!/right tail record))
                 (unsafe-set-box*! tail-box n)
                 n))
              (maybe-set-new-head!
               ;; the head may need to be reset before an element is removed
               (lambda (n)
                 (cond
                   ;; n is stored in head-box when it is the only node
                   #;((and (node-left-empty? n) (node-right-empty? n))
                      (set-box*! head-box n))
                   ((and (not (node-right-empty? n)) (node-left-empty? n))
                    (unsafe-set-box*! head-box (node-next n))))))
              (move-to-tail!
               (lambda (n)
                 (define record (node-value n))
                 (define nn (insert-record-to-tail! record))
                 (hash-set! node-cache record nn)
                 (maybe-set-new-head! n)
                 (delete-node! n)))
              (update-records!
               (lambda (record result has?)
                 (cond (has? (move-to-tail! (hash-ref node-cache record))) ;; the record registered in tbl is registered in node-cache
                       ((#,n:< (unsafe-unbox* len-box) cnt)
                        (define n (insert-record-to-tail! record))
                        (unsafe-set-box*! len-box (#,n:+ 1 (unsafe-unbox* len-box)))
                        (hash-set! tbl record result)
                        (hash-set! node-cache record n))
                       (else
                        (define h (unsafe-unbox* head-box))
                        (define n (insert-record-to-tail! record))
                        (define hv (node-value h))
                        (maybe-set-new-head! h)
                        (delete-node! h)
                        (hash-set! tbl record result)
                        (hash-set! node-cache record n)
                        (hash-remove! tbl hv)
                        (hash-remove! node-cache hv))))))
         (lambda args
           (let* ((record
                   (vector-immutable
                    #,@(map argument-info-name positional)
                    #,@(map argument-info-name positional/optional)
                    #,@(map argument-info-name keyword)
                    #,@(map argument-info-name keyword/optional)
                    #,@(if rest (list rest) '())))
                  (result (hash-ref tbl record none)))
             (if (eq? result none)
                 (let ((real-result (let () body ...)))
                   (update-records! record real-result #f)
                   real-result)
                 (let ()
                   (update-records! record result #t)
                   result))))))))

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
     100
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
  (check-equal? ((lambda/lru-cache 1 ((a 1)) (add1 a))) 2)
  (check-equal? ((lambda/lru-cache 1 ((a 1)) (add1 a)) 2) 3)
  (let* ((a 1) (proc (lambda/lru-cache 2 (b) (dynamic-wind void (lambda () a) (lambda () (set! a b))))))
    (check-equal? (proc 1) 1)
    (check-equal? (proc 2) 1)
    (check-equal? (proc 1) 1)
    (check-equal? (proc 2) 1)
    (check-equal? (proc 3) 2))

  (define-syntax-rule (mytime expr)
    (let ()
      (collect-garbage)
      (collect-garbage)
      (collect-garbage)
      (time expr)))

  ;; Benchmark
  (writeln '(time (fib 40)))
  (define r1 (mytime (fib 40)))
  (writeln '(time (fib/cached 40)))
  (define r2 (mytime (fib/cached 40)))
  (writeln '(time (fib/cached 100000)))
  (void (mytime (fib/cached 100000)))

  (check-equal? r1 r2))
