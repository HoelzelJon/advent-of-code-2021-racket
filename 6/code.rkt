(define inputs (map string->number (string-split (car (file->lines "input.txt")) ",")))

(define init-counts (build-vector 9 (lambda (n) (count (curry = n) inputs))))

(define (get-next v index)
  (case index
    [(6) (+ (vector-ref v 7) (vector-ref v 0))]
    [(8) (vector-ref v 0)]
    [else (vector-ref v (+ index 1))]))

(define (fish-step-iter v n)
  (if (= n 0) v (fish-step-iter (build-vector 9 (curry get-next v)) (- n 1))))

(define (fish-after-steps n) (apply + (vector->list (fish-step-iter init-counts n))))

; part 1
(println (fish-after-steps 80))

; part 2
(println (fish-after-steps 256))
