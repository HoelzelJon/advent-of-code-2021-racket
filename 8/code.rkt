(define inputs 
  (map 
    (compose
      (curry map (curryr string-split " ")) 
      (curryr string-split "|")) 
    (file->lines "input.txt")))

(define unique-lengths '(2 3 4 7))

(define (count-1478 l) (count (curryr member unique-lengths) (map string-length l)))

; part 1
(println (apply + (map (compose count-1478 second) inputs)))

(define true-values
  (hash
    "abcefg" 0
    "cf" 1
    "acdeg" 2
    "acdfg" 3
    "bcdf" 4
    "abdfg" 5
    "abdefg" 6
    "acf" 7
    "abcdefg" 8
    "abcdfg" 9))

(define (count-in-digits-with-length lngth lst segment)
  (count 
    (curryr string-contains? segment) 
    (filter (compose (curry equal? lngth) string-length) lst)))

(define (matches-for-length lst segment guess lngth)
  (equal? 
    (count-in-digits-with-length lngth lst segment)
    (count-in-digits-with-length lngth (hash-keys true-values) guess)))

(define (matches l segment guess)
  (andmap (curry matches-for-length l segment guess) (range 2 8)))

(define (true-value l segment)
  (car (filter (curry matches l segment) '("a" "b" "c" "d" "e" "f" "g"))))

(define (to-digit l d)
  (hash-ref true-values
    (string-append*
      (sort 
        (map 
          (curry true-value l) 
          (filter 
            (lambda (s) (not (equal? s ""))) 
            (string-split d ""))) 
        string<?))))

(define (get-line-output l)
  (string->number (string-append* (map (compose number->string (curry to-digit (car l))) (cadr l)))))

; part 2 (not the most efficient -- recalculates the counts each time, but it works)
(println (apply + (map get-line-output inputs)))
