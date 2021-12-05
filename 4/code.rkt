(define raw-input (file->lines "input.txt"))

(define called-nums (map string->number (string-split (car raw-input) ",")))

(define (unflatten l n)
  (if (empty? l)
    '()
    (cons
      (take l n)
      (unflatten (drop l n) n))))

(define boards
  (map
    (lambda (board-strs)
      (map
        (lambda (row-str)
          (map string->number
            (filter 
              (lambda (s) (not (equal? s "")))
              (string-split row-str " "))))
        (cdr board-strs)))
    (unflatten (cdr raw-input) 6)))

(define (board-delay board)
      (map
        (lambda (r)
          (map
            (lambda (n) (index-of called-nums n))
            r))
        board))

(define (board-row-delays board)
  (map (lambda (row) (argmax identity row)) board))

(define (board-col-delays board)
  (if (empty? (car board)) 
    '()
    (cons 
      (argmax identity (map car board))
      (board-col-delays (map cdr board)))))

(define (total-board-delay board)
  (let ([delays (board-delay board)])
    (min
      (argmin identity (board-row-delays delays))
      (argmin identity (board-col-delays delays)))))

(define best-board
  (argmin total-board-delay boards))

; did the scoring of the winning and losing boards by hand :P

; part 1
(println (* 1026 (list-ref called-nums (total-board-delay best-board))))

(define worst-board (argmax total-board-delay boards))

; part 2
(println (* 305 (list-ref called-nums (total-board-delay worst-board))))
