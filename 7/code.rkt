(define inputs (sort (map string->number (string-split (car (file->lines "input.txt")) ",")) <))

;; length of inputs is even, so median is avg of two elements
(define median 
  (let ([tail (list-tail inputs (quotient (length inputs) 2))])
    (quotient (+ (car tail) (cadr tail)) 2)))

(define (dist n) (abs (- n median)))

; part 1
(println (apply + (map dist inputs)))


(define (dist2 a b) (let ([diff (abs (- a b))]) (/ (* diff (+ diff 1)) 2)))

(define (total-dist2 x) (apply + (map (curry dist2 x) inputs)))

(define min-dist2 (argmin identity (map total-dist2 (range (argmin identity inputs) (argmax identity inputs)))))

; part 2 -- I tried but failed to find a nice closed-form solution for this part :/
(println min-dist2)
