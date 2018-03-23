;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snekGame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define width 500)
(define height 500)
(define diameter 10)
(define BG (empty-scene width height))

;creates snek data structure
;(define-struct snek[x y direction LoS])

;creates segment data structure
(define-struct segment[x y direction])

;x and y are numbers that represent the current x and y coordinates of the snek

;A direction is one of:
; -Up ("U")
; -Down ("D")
; -Left ("L")
; -Right ("R")

;A list of segments (LoS) is one of:
; - the empty list '()
; - (cons segment LoS)

;The world state is the list of segments (LoS)

;tock - takes in a world state, and moves the circle diameter distance, in snek-direction
;Examples:
;(cons (segment 250 250 R) '()) -> (cons (segment 260 250 R) '())
;(cons (segment 100 400 D) '()) -> (cons (segment 100 410 D) '())
;(cons (segment 300 300 L) '()) -> (cons (segment 290 300 L) '())
;(cons (segment 200 200 U) '()) -> (cons (segment 200 190 U) '())
;ws -> ws

;updated to account for new LoS ws, now has nested cond statement.
(define (tock ws)
  (cond [(empty? ws) '()]      
  [else (cond [(string=? (segment-direction (first ws)) "R") (cons (make-segment (+ (segment-x (first ws)) 10) (segment-y (first ws)) "R") (tock (rest ws)))]
        [(string=? (segment-direction (first ws)) "L") (cons (make-segment (- (segment-x (first ws)) 10) (segment-y (first ws)) "L") (tock (rest ws)))]
        [(string=? (segment-direction (first ws)) "U") (cons (make-segment (segment-x (first ws)) (- (segment-y (first ws)) 10) "U") (tock (rest ws)))]
        [(string=? (segment-direction (first ws)) "D") (cons (make-segment (segment-x (first ws)) (+ (segment-y (first ws)) 10) "D") (tock (rest ws)))]
        [else (cons (first ws) (tock (rest ws)))])]))
;even though technically else should never happen since theres always a last pressed key

;render - takes in a world state and produces an image
  ;UNSURE OF BASE CASE or this updated function in general
(define (render ws)
  (cond [(= (length ws) 1) (place-image (circle diameter "outline" "red") (segment-x (first ws)) (segment-y (first ws)) BG)]
        [else (place-image (circle diameter "outline" "red") (segment-x (first ws)) (segment-y (first ws)) (render (rest ws)))]))

;press - takes in a world state and a key and returns an updated world state
  ;updated to account for new LoS ws, now has nested cond statement.
(define (press ws ke)
  (cond [(empty? ws) '()]
        [else 
  (cond [(key=? ke "left") (cons (make-segment (segment-x (first ws)) (segment-y (first ws)) "L") (press (rest ws) ke))]
        [(key=? ke "right") (cons (make-segment (segment-x (first ws)) (segment-y (first ws)) "R") (press (rest ws) ke))]
        [(key=? ke "up") (cons (make-segment (segment-x (first ws)) (segment-y (first ws)) "U") (press (rest ws) ke))]
        [(key=? ke "down") (cons (make-segment (segment-x (first ws)) (segment-y (first ws)) "D") (press (rest ws) ke))])]))

;end - ends the game if snek hits a wall
(define (end ws)
  (boolean=? (or (= (segment-x (first ws)) width) (= (segment-x (first ws)) 0) (= (segment-y (first ws)) height) (= (segment-y (first ws)) 0)) #true))

;endpic - displays final image with text "worm hit border" in lower left corner
(define (endPic ws)
  (place-image (text "Worm Hit Border" 24 "black") 400 400 (render ws)))

  ;attempted altered version to increase efficiency 
   ; (place-image (text "Worm Hit Border" 24 "black") 400 400 (place-image (circle diameter "solid" "red") (snek-x ws) (snek-y ws) BG)))

;The main method,  
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [on-key press]
    [stop-when end endPic]))

;Launches program, starts with a snek at 250, 250 moving to the right 
(main (cons (make-segment 250 250 "R") (cons (make-segment (- 250  (* diameter 2)) 250 "R") (cons (make-segment (- 250 (* diameter 4)) 250 "R") '()))))
  

