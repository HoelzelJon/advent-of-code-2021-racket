(define inputs (map
  (lambda (s) (string-split s " ")) 
  (file->lines "input.txt")))

(struct coord (horiz vert))

(define (move c dir amount)
  (let ([h (coord-horiz c)]
        [v (coord-vert c)])
    (case dir
      [("forward") (coord (+ h amount) v)]
      [("down") (coord h (+ v amount))]
      [("up") (coord h (- v amount))])))

(define final-coord 
  (foldl 
    (lambda (strs old-c)
      (move old-c (car strs) (string->number (cadr strs))))
    (coord 0 0)
    inputs))

; part 1
(println (* (coord-horiz final-coord) (coord-vert final-coord)))


(struct coord2 (horiz vert aim))

(define (move2 c dir amount)
  (let ([h (coord2-horiz c)]
        [v (coord2-vert c)]
        [a (coord2-aim c)])
    (case dir
      [("forward") (coord2 (+ h amount) (+ v (* a amount)) a)]
      [("down") (coord2 h v (+ a amount))]
      [("up") (coord2 h v (- a amount))])))

(define final-coord2
  (foldl 
    (lambda (strs old-c)
      (move2 old-c (car strs) (string->number (cadr strs))))
    (coord2 0 0 0)
    inputs))

; part 2
(println (* (coord2-horiz final-coord2) (coord2-vert final-coord2)))
