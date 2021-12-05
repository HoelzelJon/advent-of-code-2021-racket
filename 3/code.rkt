(define inputs (map
  (lambda (s)
    (map string->number 
         (filter 
           (lambda (s2) (not (equal? s2 ""))) 
           (string-split s ""))))
  (file->lines "input.txt")))

(define (get-counts nums)
  (foldl 
    (lambda (lst1 lst2) (map + lst1 lst2))
    (build-list (length (car nums)) (lambda n 0))
    nums))

(define (get-first-count nums)
  (foldl (lambda (lst agg) (+ agg (car lst))) 0 nums))

(define (make-num low-str hi-str)
  (string->number 
    (string-append* 
      (map 
        (lambda (c) (if (> c (/ (length inputs) 2)) hi-str low-str)) 
        (get-counts inputs))) 
    2))

(define gamma (make-num "0" "1"))

(define epsilon (make-num "1" "0"))

; part 1
(println (* gamma epsilon))


(define (make-num2 low-n hi-n)
  (string->number
    (string-append* (map number->string (make-num2-rec low-n hi-n inputs)))
    2))

(define (make-num2-rec low-n hi-n lists)
  (cond
    [(empty? (car lists)) '()]
    [(equal? 1 (length lists)) (car lists)]
    [else 
      (let ([digit (if (>= (get-first-count lists) (/ (length lists) 2)) hi-n low-n)])
        (cons digit
          (make-num2-rec low-n hi-n
            (map cdr (filter (lambda (l) (equal? (car l) digit)) lists)))))]))

(define oxygen-rating (make-num2 0 1))

(define co2-rating (make-num2 1 0))

; part 2
(println (* oxygen-rating co2-rating))
