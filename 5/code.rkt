(require racket/set)

(struct line (c1 c2))

(define lines
  (map
    (lambda (line-str)
      (let 
        ([coord-list 
           (map 
             (lambda (coords-str) (map string->number (string-split coords-str ",")))
             (string-split line-str " -> "))])
        (line (car coord-list) (cadr coord-list))))
    (file->lines "input.txt")))

(define (line-x1 l) (car (line-c1 l)))
(define (line-x2 l) (car (line-c2 l)))
(define (line-y1 l) (cadr (line-c1 l)))
(define (line-y2 l) (cadr (line-c2 l)))

(define max-x (argmax identity (map (lambda (ln) (max (line-x1 ln) (line-x2 ln))) lines)))
(define max-y (argmax identity (map (lambda (ln) (max (line-y1 ln) (line-y2 ln))) lines)))

(define (horizontal? ln) (equal? (line-x1 ln) (line-x2 ln)))
(define (vertical? ln) (equal? (line-y1 ln) (line-y2 ln)))

(define (fancy-range a b) 
  (inclusive-range a b (if (> b a) 1 -1)))

; (define (points-on-diag ln) '()) ; part 1

(define (points-on-diag ln) ; part 2
  (map 
    (lambda (x y) (list x y)) 
    (fancy-range (line-x1 ln) (line-x2 ln)) (fancy-range (line-y1 ln) (line-y2 ln))))

(define (points-on ln)
  (cond
    [(vertical? ln) 
     (map 
       (lambda (x) (list x (line-y1 ln)))
       (fancy-range (line-x1 ln) (line-x2 ln)))]
    [(horizontal? ln)
     (map 
       (lambda (y) (list (line-x1 ln) y))
       (fancy-range (line-y1 ln) (line-y2 ln)))]
    [else (points-on-diag ln)]))

(define (point-update-rec lns single-hit overlaps)
  (if (empty? lns)
    overlaps
    (let* 
      ([line-pts (list->set (points-on (car lns)))]
       [new-overlaps (set-intersect line-pts single-hit)])
      (point-update-rec
        (cdr lns) 
        (set-subtract (set-union single-hit line-pts) new-overlaps)
        (set-union overlaps new-overlaps)))))

(define all-overlaps (point-update-rec lines (set) (set)))

(print (length (set->list all-overlaps)))
