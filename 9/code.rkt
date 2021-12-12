(require math/array)

(define inputs
  (list*->array
    (map
      (lambda (s) 
        (map 
          string->number 
          (filter 
            (lambda (s2) (not (equal? s2 "")) ) 
            (string-split s ""))))
      (file->lines "input.txt"))
    number?))

(define (neighbor-idxs p)
  (let ([x (vector-ref p 0)] [y (vector-ref p 1)])
    (list 
      (vector x (- y 1))
      (vector x (+ y 1))
      (vector (- x 1) y)
      (vector (+ x 1) y))))

(define (in-bounds arr p)
  (let* 
    ([x (vector-ref p 0)]
     [y (vector-ref p 1)]
     [shape (array-shape arr)]
     [w (vector-ref shape 0)]
     [h (vector-ref shape 1)])
    (and
      (>= x 0)
      (>= y 0)
      (< x w)
      (< y h))))

(define (neighbor-coords arr p) 
  (filter (curry in-bounds arr) (neighbor-idxs p)))

(define (neighbor-vals arr p)
  (map (curry array-ref arr) (neighbor-coords arr p)))

(define (is-local-min arr p)
  (andmap (curry < (array-ref arr p)) (neighbor-vals arr p)))

(define (all-coords arr)
  (let* 
    ([shape (array-shape arr)]
     [w (vector-ref shape 0)]
     [h (vector-ref shape 1)])
    (flatten
      (map 
        (lambda (x) (map (lambda (y) (vector x y)) (range 0 h)))
        (range 0 w)))))

(define (all-min-vals arr)
  (map (curry array-ref arr) (filter (curry is-local-min arr) (all-coords arr))))

; part 1
; (println (apply + (map (curry + 1) (all-min-vals inputs)))) ; TODO: uncomment

; > Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.
; This implies that all basins will be bordered entirely by 9s

(define (search q explored pts arr)
  (if (empty? q)
    explored
    (let* ([v (car q)]
           [unexplored-neighbors 
             (filter
               (lambda (n)
                 (and
                   (member n pts)
                   (not (member n explored))))
               (neighbor-coords arr v))])
      (search
        (append (cdr q) unexplored-neighbors)
        (append explored unexplored-neighbors)
        pts
        arr))))

(define (extract-clusters arr pts)
  (if (empty? pts)
    '()
    (let* ([cluster (search (list (car pts)) '() pts arr)])
      (cons 
        cluster 
        (extract-clusters
          arr 
          (filter
            (lambda (p) (not (member p cluster)))
            pts))))))

(define basin-pts
  (filter
    (lambda (p) (not (equal? 9 (array-ref inputs p))))
    (all-coords inputs)))

(define basins (extract-clusters inputs basin-pts))

(define basin-sizes (sort (map length basins) >))

; part 2
(println (* (first basin-sizes) (* (second basin-sizes) (third basin-sizes))))
