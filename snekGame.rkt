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
(define-struct snek[x y direction LoS])

;creates segment data structure
(define-struct segment[x y])

;x and y are numbers that represent the current x and y coordinates of the snek

;A direction is one of:
; -Up ("U")
; -Down ("D")
; -Left ("L")
; -Right ("R")

;A list of segments (LoS) is one of:
; - the empty list '()
; - (cons segment LoS)

;The world state is a snek

;tock - takes in a world state, and moves the circle diameter distance, in snek-direction
;Examples:
;(cons (segment 250 250 R) '()) -> (cons (segment 260 250 R) '())
;(cons (segment 100 400 D) '()) -> (cons (segment 100 410 D) '())
;(cons (segment 300 300 L) '()) -> (cons (segment 290 300 L) '())
;(cons (segment 200 200 U) '()) -> (cons (segment 200 190 U) '())
;ws -> ws

;updated to account for corrected snek ws
(define (tock ws)
  (cond [(string=? (snek-direction ws) "R") (make-snek (+ (snek-x ws) 10) (snek-y ws) "R" (updateLoS ws (snek-LoS ws) (length (snek-LoS ws))))]
       [(string=? (snek-direction ws) "L") (make-snek (- (snek-x ws) 10) (snek-y ws) "L" (updateLoS ws (snek-LoS ws) (length (snek-LoS ws))))]
       [(string=? (snek-direction ws) "U") (make-snek (snek-x ws) (- (snek-y ws) 10) "U" (updateLoS ws (snek-LoS ws) (length (snek-LoS ws))))]
       [(string=? (snek-direction ws) "D") (make-snek (snek-x ws) (+ (snek-y ws) 10) "D" (updateLoS ws (snek-LoS ws) (length (snek-LoS ws))))]
       [else ws]))
;even though technically else should never happen since theres always a last pressed key

;updateLoS - helper function for update, takes in a ws, LoS (from ws), and a number representing the length of the LoS from the ws which will be used as a counter.  It returns the updated LoS
;with a new segment added to the front based on the current ws, and the last segment removed.
(define (updateLoS ws LoS n)
  (cond [(equal? n 0) '()]
        [(equal? n (length LoS)) (cons (cond [(string=? (snek-direction ws) "R") (make-segment (+ (snek-x ws) 10) (snek-y ws))]
                                              [(string=? (snek-direction ws) "L") (make-segment (- (snek-x ws) 10) (snek-y ws))]
                                              [(string=? (snek-direction ws) "U") (make-segment (snek-x ws) (- (snek-y ws) 10))]
                                              [(string=? (snek-direction ws) "D") (make-segment (snek-x ws) (+ (snek-y ws) 10))]) (updateLoS ws LoS (sub1 n)))]
        [else (cons (first LoS) (updateLoS ws (rest LoS) (sub1 n)))]))
        
;render - takes in a world state and produces an image
  ;updated to accounted for correcred snek ws
(define (render ws)
  (cond [(= (length (snek-LoS ws)) 1) (place-image (circle diameter "solid" "red") (segment-x (first (snek-LoS ws))) (segment-y (first (snek-LoS ws))) BG)]
        [else (place-image (circle diameter "solid" "red") (segment-x (first (snek-LoS ws))) (segment-y (first (snek-LoS ws))) (render (make-snek (snek-x ws) (snek-y ws) (snek-direction ws) (rest (snek-LoS ws)))))]))

;press - takes in a ws and a key and returns an updated ws
  ;updated to account for corrected snek ws
(define (press ws ke) 
  (cond [(key=? ke "left") (make-snek (snek-x ws) (snek-y ws) "L" (snek-LoS ws))]
        [(key=? ke "right") (make-snek (snek-x ws) (snek-y ws) "R" (snek-LoS ws))]
        [(key=? ke "up") (make-snek (snek-x ws) (snek-y ws) "U" (snek-LoS ws))]
        [(key=? ke "down") (make-snek (snek-x ws) (snek-y ws) "D" (snek-LoS ws))]))

;end - ends the game if snek hits a wall
(define (end ws)
  (boolean=? (or (= (snek-x ws) width) (= (snek-x ws) 0) (= (snek-y ws) height) (= (snek-y ws) 0)) #true))

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
(main (make-snek 250 250 "R" (cons (make-segment 250 250) (cons (make-segment (- 250  (* diameter 2)) 250) (cons (make-segment (- 250 (* diameter 4)) 250) '())))))
  

