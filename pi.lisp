
;;
;; Preamble: Lisp prerequisits
;;

;; These two lines sets the number of binary digits used to represent a float
;; in Lisp. This is necessary because you'll be working with tiny numbers
;; TL;DR ignore these two lines
(setf (EXT:LONG-FLOAT-DIGITS) 35000)
(setf *read-default-float-format* 'long-float)

;; This method rounds a number to a certain precision
;; It takes two arguments: the number to round and the number of digits to
;; round in decimals
;;
;; Example: (roundToPrecision 10.0044 3) -> 10.004
(defun roundToPrecision (number precision)
  (let
    ((p (expt 10 precision)))
    (/ (round (* number p)) p)
  )
)

;;
;; Exercise
;;

;; Exercise
;; Your task is to implement the Gauss-Legendre algorithm for calculating pi
;; and extract 10.000 (ten thousand) digits

;; Gauss-Legendre algorithm on Wikipedia
;; https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_algorithm

(setf lastA 1)
(setf lastB ( / 1 ( sqrt 2 ) ) )
(setf lastT ( / 1 4.0 ) )
(setf lastP 1)

(defun nextA(prevA prevB)
  (setf newA
      ( / ( + prevA prevB ) 2 )
  )
)

(defun nextB(prevA prevB)
  (setf newB
    (sqrt (+ prevA prevB))
  )
)

(defun nextT(prevA prevT prevP)
  (setf newT
    (- prevT ( * prevP ( * (- prevA newA) (- prevA newA) ) ) )
  )
)

(defun nextP(prevP)
  (setf newP
    (* 2 prevP)
  )
)

(defun newPI(A B Tt)
  (setf pi
    (/(+ A B)(* 4 Tt))
  )
)


(defun calcPi(prevA prevB prevT prevPi)
  (setf lastA newA)
  (setf lastB newB)
  (setf lastP newP)
  (setf lastT newT)
  (setf lastPi pi)
  (nextA (lastA lastB))
  (nextB (lastA lastB))
  (nextT (lastA lastT lastP))
  (nextP (lastP))
  (newPi newA newB newT)
  roundToPrecision(lastPi 10000)
  roundToPrecision(pi 10000)
  if( ( = lastPi pi)
  (write "calculation complete")
  (calcPi lastA lastB lastT pi)
  )
)

(nextA lastA lastB)
(nextB lastA lastB)
(nextP lastP)
(nextT lastA lastT lastP)
(newPI newA newB newT)
; (write pi)
; (write newA)
; (write newB)
; (write (+ 1 newT))
