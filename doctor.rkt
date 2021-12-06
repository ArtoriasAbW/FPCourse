; "Доктор". Осень 2021
#lang scheme/base
; В учебных целях используется базовая версия Scheme

(require racket/vector)
; подключаем функции для работы с векторами
 
; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v3 name)
  )


(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (println '**)
    (car (read))
    )
  )

;упражнение 5
(define (visit-doctor-v2 stop-word patient-max)
  (let loop ((patients-remain patient-max))
    (if (> patients-remain 0)
        (let ask ((name (ask-patient-name)))
          (if (equal? name stop-word) `(time to go home)
              (begin
                (printf "Hello, ~a!\n" name)
                (print '(what seems to be the trouble?))
                (doctor-driver-loop-v2 name)
                (loop (- patients-remain 1)))))
        `(time to go home)
        )
    )
  )



; упражнение 6
(define keywords_structure '#(
                              ( ; начало данных 1й группы
                               (depressed suicide exams university) ; список ключевых слов 1й группы
                               ( ; список шаблонов для составления ответных реплик 1й группы 
                                (when you feel depressed, go out for ice cream)
                                (depression is a disease that can be treated)
                                ;
                                (you are important)
                                (what do you think about most often?)
                                ()
                                )
                               ) ; завершение данных 1й группы
                              ( ; начало данных 2й группы ...
                               (mother father parents brother sister uncle aunt grandma grandpa)
                               (
                                (tell me more about your * , i want to know all about your *)
                                (why do you feel that way about your * ?)
                                ;
                                (how old is your * ?)
                                (why do you feel that way about yuor * ?)
                                )
                               )
                              ( ; group 3
                               (university scheme lections motivation)
                               (
                                (your education is important)
                                (how much time do you spend on your studies ?)
                                ;
                                (maybe you should change the direction of your studies)
                                (what is your main problem with * ?)
                                )
                               )
                              ( ; group 4
                               (diet fat weight bulimia anorexia thinness food)
                              (
                               (how often do you have problems with eating?)
                               (does anybody else know about that?)
                               (you should count the number of calories you eat)
                               ))
                              ( ; group 5
                               (nightmare sleep loneliness fear terror dread)
                              (
                               (how often do you have problems with dreaming?)
                                       (did you have the same problems in your childhood ?)
                                       (is your bed comfortable enough ?))
                              )))

; упражнение 6
(define (visit-doctor-v3 stop-word patient-max)
  (let loop ((patients-remain patient-max))
    (if (> patients-remain 0)
        (let ask ((name (ask-patient-name)))
          (if (equal? name stop-word) `(time to go home)
              (begin
                (printf "Hello, ~a!\n" name)
                (print '(what seems to be the trouble?))
                (doctor-driver-loop-v3 name)
                (loop (- patients-remain 1)))))
        `(time to go home)
        )
    )
  )


; упражнение 6
(define (doctor-driver-loop-v3 name)
  (let loop ((doctor-memory #()))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
         (printf "Goodbye, ~a!\n" name)
         (print '(see you next week)))
        (else (print (reply-ex6 user-response doctor-memory)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
              (loop (vector-append (vector user-response) doctor-memory))
              )
        )
      )
    )
  )


; упражнение 6
(define (reply-ex6 user-response doctor-memory)
  (case (random 4)
    ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ((2) (if (vector-empty? doctor-memory) (reply hedge) (history-answer doctor-memory)))
    ((3) (if (check-for-keywords user-response) (answer-by-keyword user-response) (hedge)))
    )
  )


;упражнение 6
; возвращает список всех ключевых слов
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
(ormap (lambda (x) (member x all-keywords)) phrase)
)


; упражнение 6
(define (answer-by-keyword phrase)
  ; returns list of all keywords of phrase and its length (lenght <keywords>)
  (define get-all-keywords
    (foldl (lambda (curr res) (if (member curr all-keywords) (cons (+ 1 (car res)) (cons curr (cdr res))) res))
         (cons 0 `()) phrase)
    )


  ; gets all answers and their number (from possible) (length <answers>)
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

  ; gets random answer from all possible
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
(define (visit-doctor-ex7 stop-word patient-max)
  (let loop ((patients-remain patient-max))
    (if (> patients-remain 0)
        (let ask ((name (ask-patient-name)))
          (if (equal? name stop-word) `(time to go home)
              (begin
                (printf "Hello, ~a!\n" name)
                (print '(what seems to be the trouble?))
                (doctor-driver-loop-ex7 name)
                (loop (- patients-remain 1)))))
        `(time to go home)
        )
    )
  )


(define (doctor-driver-loop-ex7 name)
  (let loop ((doctor-memory #()))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
         (printf "Goodbye, ~a!\n" name)
         (print '(see you next week)))
        (else (print (reply-ex7 user-response doctor-memory)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
              (loop (vector-append (vector user-response) doctor-memory))
              )
        )
      )
    )
  )


(define (reply-ex7 user-response doctor-memory)
  (case (random 4)
    ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ((2) (if (vector-empty? doctor-memory) (reply hedge) (history-answer doctor-memory)))
    ((3) (if (check-for-keywords user-response) (answer-by-keyword user-response) (hedge)))
    )
  )

;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
; цикл диалога Доктора с пациентом
; параметр name -- имя пациента

(define (doctor-driver-loop name)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
    (cond 
      ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
       (printf "Goodbye, ~a!\n" name)
       (print '(see you next week)))
      (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
            (doctor-driver-loop name)
            )
      )
    )
  )


;упражнение 4
(define (doctor-driver-loop-v2 name)
  (let loop ((doctor-memory #()))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
         (printf "Goodbye, ~a!\n" name)
         (print '(see you next week)))
        (else (print (reply-with-memory user-response doctor-memory)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
              (loop (vector-append (vector user-response) doctor-memory))
              )
        )
      )
    )
  )


;упражнение 4
(define (reply-with-memory user-response doctor-memory)
  (case (random 3)
    ((0) (qualifier-answer user-response))
    ((1) (hedge))
    ((2) (if (vector-empty? doctor-memory) (reply user-response) (history-answer doctor-memory)))
    )
  )

;упражнение 4
(define (history-answer doctor-memory)
  (append '(earlier you said that) (change-person (pick-random-vector doctor-memory)))
  )

; генерация ответной реплики по user-response -- реплике от пользователя ы
(define (reply user-response)
  (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
    ((0) (qualifier-answer user-response)) ; 1й способ
    ((1) (hedge))  ; 2й способ
    )
  )
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
  (append (pick-random-vector '#((you seem to think that)
                                 (you feel that)
                                 (why do you believe that)
                                 (why do you say that)
                                 ;упражнение1
                                 (why do you think that)
                                 (you said that)
                                 (do you friends know that))
                              )
          (change-person user-response)
          )
  )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
  )

; замена лица во фразе			
(define (change-person phrase)
  (many-replace-v2 '((am are)
                     (are am)
                     (i you)
                     (me you)
                     (mine yours)
                     (my your)
                     (myself yourself)
                     (you i)
                     (your my)
                     (yours mine)
                     (yourself myself)
                     (we you)
                     (us you)
                     (our your)
                     (ours yours)
                     (ourselves yourselves)
                     (yourselves ourselves)
                     (shall will))
                   phrase)
  )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (cond ((null? lst) lst)
        (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                          (car lst) ; иначе в начале ответа помещается прежнее начало списка без изменений
                          )
                      (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                      )
                )
              )
        )
  )


;упражнение 2
(define (many-replace-v2 replacement-pairs lst)
  (let loop ((rest lst) (result '()))
    (if (null? rest) (reverse result)
        (let ((repl-word (assoc (car rest) replacement-pairs)))
          (loop (cdr rest)
                (cons (if repl-word (cadr repl-word)
                          (car rest))
                      result))
          )
        )
    )
  )

;упражнение 3
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (word) (let ((repl-word (assoc word replacement-pairs))) (if repl-word (cadr repl-word) word))) lst)
  )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pick-random-vector '#((please go on)
                         (many people have the same sorts of feelings)
                         (many of my patients have told me the same thing)
                         (please continue)
                         ;упражнение 1
                         (ahahaha are you serious?)
                         (*doctor yawns*)
                         (*doctor looks at the clock*))
                      )
  )


