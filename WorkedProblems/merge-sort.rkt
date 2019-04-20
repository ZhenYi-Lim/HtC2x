;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname merge-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;merge sort

;; (listof Number) -> (listof Number)
;; produces sorted list in ascending order using merge sort
(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 2)) (list 2))
(check-expect (merge-sort (list 1 2)) (list 1 2))
(check-expect (merge-sort (list 4 3)) (list 3 4))
(check-expect (merge-sort (list 6 5 3 1 8 7 2 4)) (list 1 2 3 4 5 6 7 8))
;(define (merge-sort lon) lon) ;stub
(define (merge-sort lon)
  (cond [(empty? lon) empty]
        [(empty? (rest lon)) lon]
        [else                         
         (merge (merge-sort (take lon (quotient (length lon) 2)))
                (merge-sort (drop lon (quotient (length lon) 2))))]))

;; (listof Number) (listof Number) -> (listof Number)
;; merge two lists into a single in ascending order.
;; ASSUME: both lists are already sorted.
(check-expect (merge empty empty) empty)
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge empty (list 2)) (list 2))
(check-expect (merge (list 1 3 5 6) (list 2 4 7 8)) (list 1 2 3 4 5 6 7 8))
(define (merge lsta0 lstb0)
  (local [(define (merge lsta lstb rsf)
            (cond [(empty? lsta) (append rsf lstb)]
                  [(empty? lstb) (append rsf lsta)]
                  [(< (first lsta) (first lstb))
                   (merge (rest lsta) lstb (append rsf (list (first lsta))))]
                  [else
                   (merge lsta (rest lstb) (append rsf (list (first lstb))))]))]
    (merge lsta0 lstb0 empty)))

;; (listof Number) Natural -> (listof Number)
;; produce list of first n elements of lon/ list after dropping first n elements
;; ASSUME: lon has at least n elements
(check-expect (take empty 0) empty)
(check-expect (take (list 1 2 3 4) 0) empty)
(check-expect (take (list 1 2 3 4) 2) (list 1 2))
(check-expect (drop empty 0) empty)
(check-expect (drop (list 1 2 3 4) 0) (list 1 2 3 4))
(check-expect (drop (list 1 2 3 4) 2) (list 3 4))

(define (take lon0 n)
  (local [(define (take lon rsf)
            (cond [(empty? lon) rsf] ;should never get here
                  [(= n (length rsf)) rsf]
                  [else
                   (take (rest lon) (append rsf (list (first lon))))]))]
    (take lon0 empty)))

(define (drop lon n)
  (cond [(empty? lon) empty] ;should never get here
        [(zero? n) lon]
        [else
         (drop (rest lon) (sub1 n))]))