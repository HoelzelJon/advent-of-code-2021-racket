(define inputs 
  (map string->number (file->lines "input.txt")))

(define (count-increases nums) 
  (if (empty? (cdr nums))
    0
    (+ (if (> (cadr nums) (car nums)) 1 0)
       (count-increases (cdr nums)))))

; part 1
(println (count-increases inputs))

(define (smooth nums)
  (if (< (length nums) 3)
    '()
    (cons 
      (+ (first nums) (second nums) (third nums))
      (smooth (cdr nums)))))

; part 2
(println (count-increases (smooth inputs)))
