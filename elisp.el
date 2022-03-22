
(* 45 3)


(+ 1 #xFF)
(quote (1 2 3 4))
'(1 2 3 4)
(defun mul-5 (number)
  (* number 5))
(mul-5 5)

(mapcar 'mul-5 '(1 2 3 4))
(cdr '(a b c))

(if (= 3 4)
    (message "True")
  (message "False"))
(concat "Hello, " "world")
(string= "foo" "foo")
(upcase "jeffrey")
(substring "jeffrey" 0 4)
(setq a 8)
;;(setq a (read-number "Enter a number: "))
(if (= a 4)
    (progn
      (let ((b (* a 2)))
	(insert (format "\n%d" a))
	(message "%d" (mul-5 b))))
  (message "Not equal to four"))

;;(message "String is %s" (read-string "Your name? "))
(setq number 5)
(setq guess 155)
(while (/= guess number)
  (progn
    (setq guess 5);(read-number "Enter your guess: "))
    (if (= guess number)
	(message "Correct!")
      (message "Nope, try again"))))

(defun pick-from-list ()
  (interactive)
  (let ((choices '("cat" "dog" "Parrot" "hamster")))
    (message "%s" (ido-completing-read "Pick one:" choices))))
;(if (y-or-n-p "Run code?")
;    (progn
;      (message "Code ran")
;      (insert "(message \"code\")" ))
;  (progn
;    (message "Didn't run it")))
(defun name-and-age (x y)
  ;;The newline between the two strings actually made a difference. Didn't expect that
  (interactive "sEnter your name: 
nEnter your age: ")
  (message "Hi, %s, who is %d years old" x y))
(shell-command-to-string "ls")
(let ((l 54)
      (m 45))
  (* l m))

(defun inc (n)
  (+ n 1))
(1+ 3)
(defun lower-or-higher (n number)
  "Tells you whether or not one number is lower or higher than a number, or if they're the same"
  (cond ((< n number) (message "Your guess is less than the actual number" n number))
	 ((> n number) (message "Your guess is greater than the actual number"))
	 ((= n number) (message "Correct!"))))

(defun guess-game ()
  "A really basic guessing game so that I could learn some elisp"
  (interactive)
  (let ((number (random 100))
	(guess 0)
	(number-of-guesses 0))
    (message "Hello!")
    (sleep-for .5)
    (while (/= guess number)
      (setq guess (read-number "Enter your guess: "))
      (setq number-of-guesses (1+ number-of-guesses))
      (lower-or-higher guess number)
      (sleep-for 1))
    (message "It took you %d guesses to get the right number" number-of-guesses)))

(mapcar
 (lambda (x)
   (* x 2))
 '(1 4 5 9))

(defun reverse-list (my-list)
  "Takes a list and returns the reverse. Aka the last element becomes the first."
  (let ((new-list '()))
	(while (> (length my-list) 0)
	  (push (pop my-list) new-list))
	new-list))


(defun last-elem (my-list)
  (if (>= 1 (length my-list)) (car my-list)
      (last-elem-recur (cdr my-list))))
(defun penultimate-elem (my-list)
  (if (>= 2 (length my-list)) (car my-list)
    (penultimate-elem (cdr my-list))))


(last-elem '(1 2 3 4 99))
(last-elem '(5 6 47))

(penultimate-elem '(1 2 3 4 5))
(penultimate-elem '())
(last-elem-recur '())
(round (sqrt 33))
(length '())
(reverse-list '(1 2 3 4 5))
(reverse-list '(a b c d e f g))
(car '())
(cons (car '(1 2)) '())
(reverse-list '())
(null '())
