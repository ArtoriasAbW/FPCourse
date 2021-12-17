#lang scheme

(require racket/vector)
; подключаем функции для работы с векторами
 
; основная функция, запускающая "Доктора"
; параметр name -- имя пациента

(define (ask-patient-name)
  (begin
    (printf "next!\n")
    (printf "who are you?\n")
    (print '**)
    (car (filter non-empty-string? (string-split (read-line) r_word)))
    )
  )


; упражнение 6
(define keywords_structure '#(
                              ( ; начало данных 1й группы
                               ("depressed" "suicide" "exams" "university") ; список ключевых слов 1й группы
                               ( ; список шаблонов для составления ответных реплик 1й группы 
                                ("when you feel depressed, go out for ice cream")
                                ("depression is a disease that can be treated")
                                ;
                                ("you are important")
                                ("what do you think about most often?")
                                ()
                                )
                               ) ; завершение данных 1й группя
                              ( ; начало данных 2й группы ...
                               ("mother" "father" "parents" "brother" "sister" "uncle" "aunt" "grandma" "grandpa")
                               (
                                ("tell me more about your "*", i want to know all about your" *)
                                ("why do you feel that way about your "*"?")
                                ;
                                ("how old is your"*"?")
                                ("why do you feel that way about your"*"?")
                                )
                               )
                              ( ; группа 3
                               ("university" "scheme" "lections" "motivation")
                               (
                                ("your education is important")
                                ("how much time do you spend on your studies?")
                                ;
                                ("maybe you should change the direction of your studies")
                                ("what is your main problem with "*" ?")
                                )
                               )
                              ( ; группа 4
                               ("diet" "fat" "weight" "bulimia" "anorexia" "thinness" "food")
                               (
                                ("how often do you have problems with eating?")
                                ("does anybody else know about that?")
                                ("you should count the number of calories you eat")
                                ))
                              ( ; группа 5
                               ("nightmare" "sleep" "loneliness" "fear" "terror" "dread")
                               (
                                ("how often do you have problems with dreaming?")
                                ("did you have the same problems in your childhood ?")
                                ("is your bed comfortable enough ?"))
                               )))





; упражнение 6
(define (reply-ex6 user-response doctor-memory)
  (case (random 4)
    ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ((2) (if (vector-empty? doctor-memory) (hedge) (history-answer doctor-memory)))
    ((3) (if (check-for-keywords user-response) (answer-by-keyword user-response) (hedge)))
    )
  )

(define all-keywords
  (let loop ((result `()) (i (- (vector-length keywords_structure) 1)))
    (if (< i 0)
        result
        (loop (append (car (vector-ref keywords_structure i)) result) (- i 1))
        )
    )
  )

; упражнение 6
; есть ли в фразе слово, которое есть в списке ключевых
(define (check-for-keywords phrase)
  (ormap (lambda (y) (ormap (lambda (x) (member x all-keywords)) y)) phrase)
  )


; упражнение 6
(define (answer-by-keyword phrase)

  ; список всех ключевых слов, присутствующих во фразе + его длина (lenght <keywords>)
  (define get-all-keywords
    (foldl (lambda (curr1 res1)
             (let ((tmp_res (foldl (lambda (curr2 res2) (if (member curr2 all-keywords)
                                                            (cons (+ 1 (car res2)) (cons curr2 (cdr res2)))
                                                            res2))
                                   (cons 0 `())
                                   curr1)))
               (cons (+ (car tmp_res) (car res1)) (append (cdr tmp_res) (cdr res1)))))
           (cons 0 `())
           phrase)
    )


  ; список ответов и его длина (length <answers>)
  (define (get-possible-answers-by-keyword keyword)
    (let loop ((result (cons 0 `())) (i (- (vector-length keywords_structure) 1)))
      (cond ((< i 0) result)
            (else (let ((curr-keywords (car (vector-ref keywords_structure i))) (curr-templates (cadr (vector-ref keywords_structure i))))
                    (cond ((member keyword curr-keywords)
                           (loop (cons (+ (length curr-templates) (car result)) (append curr-templates (cdr result))) (- i 1)))
                          (else (loop result (- i 1)))
                          )
                    )
                  )
            )
      )
    )

  ; выбор рандомного ответа из возможных по ключевому слову
  (define (get-answer-by-keyword keyword)
    (pick-random-list (get-possible-answers-by-keyword keyword))
    )

  (define (pick-random-list length-lst)
    (list-ref (cdr length-lst) (random (car length-lst)))
    )
  
  (let get-ans ((keyword (pick-random-list get-all-keywords))) ; выбираем рандомное ключевое слово
    (many-replace-v3 (list (list `* keyword)) (get-answer-by-keyword keyword)) ; выбираем рандомный ответ по этому ключевому слову
    )
  )

;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

; упражнение 7
(define (visit-doctor stop-word patient-max)
  (let loop ((patients-remain patient-max))
    (if (> patients-remain 0)
        (let ask ((name (ask-patient-name)))
          (if (equal? name stop-word) (printf "time to go home\n")
              (begin
                (printf "Hello, ~a!\n" name)
                (printf "what seems to be the trouble?\n")
                (doctor-driver-loop-ex7 name)
                (loop (- patients-remain 1)))))
        (printf "time to go home")
        
        )
    )
  )

(define sent_end  #px"\\.|\\?|!")
(define r_word  #px"\\s*\\b\\s*")

(define (read-response response)
  (map (lambda (x)
         (filter non-empty-string? (string-split x r_word))
         )
       (string-split response sent_end)
       )
  )

(define (merge-text lst)
  (let loop ((tmp_lst lst) (res ""))
    (if (null? tmp_lst)
        res
        (loop (cdr tmp_lst) (string-append (if (or
                                                (eq? res "")
                                                (member (car tmp_lst) (list "." "," ";" ":" "-" "?" "!"))
                                                )
                                               res
                                               (string-append res " ")
                                               ) (car tmp_lst)
                                                 )
              )
        )
    )
  )

(define (doctor-driver-loop-ex7 name)
  (let loop ((doctor-memory #()))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read-response (read-line))))
      (cond
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
         (printf "Goodbye, ~a!\n" name)
         (print '(see you next week))
         (newline))
        (else (printf (merge-text (reply-ex7 user-response doctor-memory strategies_structure))) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
              (loop (vector-append (list->vector user-response) doctor-memory))
              )
        )
      )
    )
  )

; функция предик о применимости стратегии, вес стратегии, тело стратегии (функция)

(define strategies_structure
  (vector (vector (lambda (user-response prev-responses) #t)
                  2
                  (lambda (user-response prev-responses) (hedge))
                  )
          (vector (lambda (user-response prev-responses) #t)
                  3
                  (lambda (user-response prev-responses) (qualifier-answer user-response))
                  )
          (vector (lambda (user-response prev-responses) (not (vector-empty? prev-responses)))
                  5
                  (lambda (user-response prev-responses) (history-answer prev-responses))
                  )
          (vector (lambda (user-response prev-responses) (check-for-keywords user-response))
                  10
                  (lambda (user-response prev-responses) (answer-by-keyword user-response))
                  )
                                    
          )
  )
(define (get-strategy-pred strat)
  (vector-ref strat 0)
  )

(define (get-strategy-weight strat)
  (vector-ref strat 1)
  )

(define (get-strategy-reply-func strat)
  (vector-ref strat 2)
  )

(define (reply-ex7 user-response prev-responses all-strats)
  (define (get-total-strategies-weight strategies) ; суммарный вес всех стратегий
    (let loop ((sum 0) (i (- (vector-length  strategies) 1)))
      (if (< i 0)
          sum
          (loop (+ sum (get-strategy-weight (vector-ref strategies i))) (- i 1))
          )
      )
    )
  
  (define (choose-strategy strategies)
    (let ((rand_ind (random (get-total-strategies-weight strategies))))
      (let loop ((more_weight rand_ind) (i 0))
        (let* ((strat (vector-ref strategies i)) (weight (get-strategy-weight strat)))
          (if (< more_weight weight)
              (get-strategy-reply-func strat)
              (loop (- more_weight weight) (+ i 1))
              )
          )
        )
      )
    )
  (let* ((good-strategies (vector-filter (lambda (x) ((get-strategy-pred x) user-response prev-responses)) all-strats)) ; отбираем подходящие стратегии
         (curr-strat (choose-strategy good-strategies))) ; выбираем стратегию
    (curr-strat user-response prev-responses)
    )
  )

;упражнение 4
(define (history-answer doctor-memory)
  (cons "earlier you said that" (change-person (pick-random-vector doctor-memory)))
  )

			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
  (append (pick-random-vector '#(("you seem to think that")
                                 ("you feel that")
                                 ("why do you believe that")
                                 ("why do you say that")
                                 ;упражнение1
                                 ("why do you think that")
                                 ("you said that")
                                 ("do you friends know that"))
                              )
          (change-person (list-ref user-response (random (length user-response))))
          )
  )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
  )

; замена лица во фразе			
(define (change-person phrase)
  (many-replace-v3 '(("am" "are")
                     ("are" "am")
                     ("i" "you")
                     ("me" "you")
                     ("mine" "yours")
                     ("my" "your")
                     ("myself" "yourself")
                     ("you" "i")
                     ("your" "my")
                     ("yours" "mine")
                     ("yourself" "myself")
                     ("we" "you")
                     ("us" "you")
                     ("our" "your")
                     ("ours" "yours")
                     ("ourselves" "yourselves")
                     ("yourselves" "ourselves")
                     ("shall" "will"))
                   phrase)
  )
  


;упражнение 3
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (word) (let ((repl-word (assoc word replacement-pairs))) (if repl-word (cadr repl-word) word))) lst)
  )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pick-random-vector '#(("please go on")
                         ("many people have the same sorts of feelings")
                         ("many of my patients have told me the same thing")
                         ("please continue")
                         ;упражнение 1
                         ("ahahaha are you serious?")
                         ("*doctor yawns*")
                         ("*doctor looks at the clock*"))
                      )
  )


