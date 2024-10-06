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

              (len-box (box 0))

              (insert-record-to-tail!
               (lambda (record)
                 (define tail (unsafe-unbox* tail-box))
                 (unsafe-set-box*! tail-box (insert-node!/right tail record))))
              (maybe-set-new-head!
               ;; the head may need to be reset before an element is removed
               (lambda (n)
                 (cond
                   ;; n is stored in head-box when it is the only node
                   #;((and (node-left-empty? n) (node-right-empty? n))
                      (set-box*! head-box n))
                   ((and (not (node-right-empty? n)) (node-left-empty? n))
                    (unsafe-set-box*! head-box (node-next n))))))
              (update-records!
               (lambda (record result has?)
                 (cond (has?
                        ;; it is much faster to use tail-box than to use head-box here
                        ;; because node-last is much faster than node-next
                        (let loop ((n (unsafe-unbox* tail-box)))
                          (let ((v (node-value n)))
                            (cond ((equal? v record)
                                   (insert-record-to-tail! record)
                                   (maybe-set-new-head! n)
                                   (delete-node! n))
                                  (else (loop (node-last n)))))))
                       ((#,n:< (unsafe-unbox* len-box) cnt)
                        (insert-record-to-tail! record)
                        (unsafe-set-box*! len-box (#,n:+ 1 (unsafe-unbox* len-box)))
                        (hash-set! tbl record result))
                       (else
                        (define h (unsafe-unbox* head-box))
                        (insert-record-to-tail! record)
                        (maybe-set-new-head! h)
                        (delete-node! h)
                        (hash-set! tbl record result)
                        (hash-remove! tbl (node-value h)))))))
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
     20
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

  (define-syntax-rule (mytime expr)
    (let ()
      (collect-garbage)
      (collect-garbage)
      (collect-garbage)
      (time expr)))

  ;; Benchmark
  (writeln '(time (fib 40)))
  (void (mytime (fib 40)))
  (writeln '(time (fib/cached 40)))
  (void (mytime (fib/cached 40)))
  (writeln '(time (fib/cached 40000)))
  (void (mytime (fib/cached 40000))))
