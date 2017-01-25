(define *grupo* 1)

(require (lib "misc.ss" "swindle")
         (lib "list.ss")
         (lib "lset.ss" "srfi" "1")
         (lib "trace.ss"))


;;
;; Ficheiro com código a testar
;;
;; Nota: o ficheiro tem que que estar na mesma directoria ou 
;;       então tem que ser fornecido o caminho até ao ficheiro.

;;
;; Grupo a testar
;;

(load (string-append "FP1011-parte2-grupo" (number->string *grupo*) ".scm"))
(require "modulo-aux.zo")

(define primeiras-pecas-jogador-preto
  (list (faz-pos 0 0) (faz-pos 7 7)(faz-pos 3 3) (faz-pos 4 4)))


(define jogador-grupo (string->symbol 
                       (string-append "cria-jogador" (number->string *grupo*))))
(define jogador-grupo-lambda (eval jogador-grupo))

(define cria-jogador jogador-grupo)

(define (set-jogador-lambda! fn)
  (eval `(set! ,jogador-grupo ,fn)))

(define (cria-jogador-jogada jogadas)  
  (lambda (cor)
    (define (proc1 . args)
      (let ((jogada (first jogadas)))
        (set! jogadas (rest jogadas))
        jogada))
    (define (proc2 . args)
      args)
    (lambda (m)
      (if (eq? m 'joga) 
          proc1
          proc2))))


;; Interface

(define dh-inicio-c 0)
(define (dh-inicio)
  (set! dh-inicio-c (add1 dh-inicio-c)))

(define dh-pede-primeiras-pecas-c 0)
(define (dh-pede-primeiras-pecas jogador)
  (set! dh-pede-primeiras-pecas-c (add1 dh-pede-primeiras-pecas-c)))

(define dh-executa-jogada-c 0)
(define dh-executa-jogada-jogada 0)
(define dh-executa-jogada-caps 0)
(define (dh-executa-jogada jogada lista-capturadas)
  (cond ((= dh-executa-jogada-c 0) 
         (set! dh-executa-jogada-jogada (list jogada))
         (set! dh-executa-jogada-caps (list lista-capturadas)))
        (else 
         (set! dh-executa-jogada-jogada 
               (append dh-executa-jogada-jogada (list jogada)))
         (set! dh-executa-jogada-caps 
               (append dh-executa-jogada-caps (list lista-capturadas)))))
  (set! dh-executa-jogada-c (add1 dh-executa-jogada-c)))


(define dh-aceita-jogada-c 0)
(define (dh-aceita-jogada)
  (set! dh-aceita-jogada-c (add1 dh-aceita-jogada-c)))

(define dh-termina-jogo-c 0)
(define dh-termina-jogo-venc 0)
(define (dh-termina-jogo vencedor)
  (set! dh-termina-jogo-venc vencedor)
  (set! dh-termina-jogo-c (add1 dh-termina-jogo-c)))

(define dh-assinala-erro-c 0)
(define dh-assinala-erro-jog 0)
(define dh-assinala-erro-jogada 0)
(define (dh-assinala-erro jog jogada)
  (set! dh-assinala-erro-jog jog)
  (set! dh-assinala-erro-jogada jogada)
  (set! dh-assinala-erro-c (add1 dh-assinala-erro-c)))

(define contadores
  '(dh-inicio-c 
    dh-pede-primeiras-pecas-c 
    dh-executa-jogada-c 
    dh-executa-jogada-jogada 
    dh-executa-jogada-caps 
    dh-aceita-jogada-c 
    dh-termina-jogo-c 
    dh-termina-jogo-venc 
    dh-assinala-erro-c 
    dh-assinala-erro-jog 
    dh-assinala-erro-jogada))

(define (reset-contadores!)
  (dolist (c contadores)
          (eval `(set! ,c 0))))

(define (contadores-a-zero? lst)
  (eval (cons 'and (map (lambda (cont)
                          (zero? (eval cont)))
                        lst))))


;; TAI teste
(define (cria-teste nome forma erro?)
  (cons erro? (cons nome forma)))

;; Nome do teste
(define (nome-teste teste)
  (cadr teste))

;; Código a ser avaliado
(define (forma-teste teste)
  (cddr teste))

;; Indica se a avaliação do código deve gerar um erro de execução
(define (erro?-teste teste)
  (car teste))


;; Definição dos testes
(define *ts*
  (list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TAI TABULEIRO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
      (cria-teste "teste tabuleiro 01" 
               '(tabuleiro-inicial) #f)

      (cria-teste "teste tabuleiro 02" 
               '(let ((tab (tabuleiro-inicial))
                      (res #t)
                      (l 0)
                      (c 0))
                  (while (and res (< l 8))
                            (while (and res (< c 8))
                                   (when (or (and (even? (+ l c))
                                                  (not 
                                                   (eq? (conteudo-tabuleiro 
                                                         tab (faz-pos l c))
                                                        'p)))
                                             (and (odd? (+ l c))
                                                  (not 
                                                   (eq? (conteudo-tabuleiro 
                                                         tab (faz-pos l c))
                                                      'b))))
                                     (set! res #f)
                                     (display "RES #f") (newline))
                                   (set! c (add1 c)))
                            (set! c 0)
                            (set! l (add1 l)))
                  res) #f)
      
            (cria-teste "teste tabuleiro 03" 
               '(let ((tab (tabuleiro-inicial)))
                  (coloca-peca! tab (faz-pos 5 5) 'v)
                  (eq? (conteudo-tabuleiro tab (faz-pos 5 5)) 'v))                 
                  #f)

            (cria-teste "teste tabuleiro 04" 
               '(let ((tab (tabuleiro-inicial)))
                  (retira-peca! tab (faz-pos 5 5) )
                  (eq? (conteudo-tabuleiro tab (faz-pos 5 5)) 'v))                 
                  #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;JOGADOR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
   (cria-teste "teste jogador 01" 
               '(procedure? (eval cria-jogador)) #f)
   
   (cria-teste "teste jogador 02" 
               '(procedure? ((eval cria-jogador) 'p)) #f)
   
   (cria-teste "teste jogador 03" 
               '(procedure? ((eval cria-jogador) 'b)) #f)
   
   ; tira-primeira-peca   
   (cria-teste "teste jogador 04"  
               '(let* ((j ((eval cria-jogador) 'p))
                       (pos (em j 'tira-primeira-peca)))
                  (member pos primeiras-pecas-jogador-preto)) #f)
   
   (cria-teste "teste jogador 05" 
               '(let* ((j ((eval cria-jogador) 'b))
                       (pos (em j 'tira-primeira-peca (faz-pos 3 3))))
                  (adjacentes? pos (faz-pos 3 3))) #f)
   
   (cria-teste "teste jogador 06" 
               '(let* ((j ((eval cria-jogador) 'b))
                       (pos (em j 'tira-primeira-peca (faz-pos 0 0))))
                  (adjacentes? pos (faz-pos 0 0))) #f)
   
   ; joga 
   (cria-teste "teste jogador 07" 
               '(let* ((j ((eval cria-jogador) 'b))
                       (pos-prim-aut (em j 'tira-primeira-peca (faz-pos 0 0)))
                       (tab (tabuleiro-inicial))
                       (jogada-hum null)
                       (jogada-aut null))
                  (retira-peca! tab (faz-pos 0 0))
                  (retira-peca! tab pos-prim-aut)
                  (set! jogada-hum (primeiro (dh-jogadas-possiveis tab 'p)))
                  (em j 'humano-jogou jogada-hum)
                  (dh-actualiza-tab-jogada! tab jogada-hum 'p)
                  (set! jogada-aut (em j 'joga))
                  (dh-jogada-valida? tab jogada-aut 'b)) #f)
     
   (cria-teste "teste jogador 08" 
               '(let* ((j ((eval cria-jogador) 'p))
                       (pos-prim-aut (em j 'tira-primeira-peca))
                       (pos-prim-hum (primeiro (adjacentes pos-prim-aut)))
                       (tab (tabuleiro-inicial))
                       (jogada-aut null))
                  (em j 'humano-tirou pos-prim-hum)
                  (retira-peca! tab pos-prim-aut)
                  (retira-peca! tab pos-prim-hum)
                  (set! jogada-aut (em j 'joga))
                  (dh-jogada-valida? tab jogada-aut 'p)) #f)
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONTROLE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
      (cria-teste "teste controle 01"
               '(begin (damas-havaianas)
                       (procedure? controle)) #f)
   
   (cria-teste "teste controle 02"
               '(begin (reset-contadores!)
                       (damas-havaianas)
                       (and
                        (= dh-inicio-c 1)
                        (contadores-a-zero? 
                         (lset-difference eq? contadores '(dh-inicio-c)))
                        )) #f)
   
   (cria-teste "teste controle 03"
               '(begin (reset-contadores!)
                       (damas-havaianas)
                       (em controle 'humano-escolheu-cor 'p)
                       (and
                        (= dh-inicio-c dh-pede-primeiras-pecas-c 1)
                        (contadores-a-zero? 
                         (lset-difference eq? contadores 
                                          '(dh-inicio-c
                                            dh-pede-primeiras-pecas-c)))
                        )) #f)
   
   (cria-teste "teste controle 04"
               '(begin (reset-contadores!)
                       (damas-havaianas)
                       (em controle 'humano-escolheu-cor 'p)
                       (em controle 'primeiras-pecas (faz-pos 0 0) (faz-pos 0 1))
                       (and
                        (= dh-inicio-c dh-pede-primeiras-pecas-c dh-aceita-jogada-c 1)
                        (contadores-a-zero? 
                         (lset-difference eq? contadores 
                                          '(dh-inicio-c
                                            dh-aceita-jogada-c
                                            dh-pede-primeiras-pecas-c)))
                        )) #f)
   
   (cria-teste "teste controle 05"
               '(begin (reset-contadores!)
                       (set-jogador-lambda! 
                        (cria-jogador-jogada 
                         (list (faz-jogada (faz-pos 2 1)(faz-pos 0 1)))))
                       (damas-havaianas)
                       (em controle 'humano-escolheu-cor 'p)
                       (em controle 'primeiras-pecas (faz-pos 0 0) (faz-pos 0 1))
                       (em controle 'recebe-jogada-humano 
                           (faz-jogada (faz-pos 2 0) (faz-pos 0 0)))
                       (and
                        (= dh-inicio-c dh-pede-primeiras-pecas-c 1)
                        (= dh-executa-jogada-c dh-aceita-jogada-c 2)
                        (equal? (first dh-executa-jogada-caps) (list (faz-pos 1 0)))
                        (equal? (first dh-executa-jogada-jogada) 
                                (faz-jogada (faz-pos 2 0) (faz-pos 0 0)))
                        (equal? (second dh-executa-jogada-caps) (list (faz-pos 1 1)))
                        (equal? (second dh-executa-jogada-jogada) 
                                (faz-jogada (faz-pos 2 1)(faz-pos 0 1)))
                        (contadores-a-zero? 
                         (lset-difference eq? contadores 
                                          '(dh-inicio-c
                                            dh-pede-primeiras-pecas-c
                                            dh-aceita-jogada-c
                                            dh-executa-jogada-c
                                            dh-executa-jogada-caps
                                            dh-executa-jogada-jogada)))
                        )) #f)
   
     
   (cria-teste "teste controle 06"
               '(begin (reset-contadores!)
                       (set-jogador-lambda! 
                        (cria-jogador-jogada 
                         (list (faz-jogada (faz-pos 2 0) (faz-pos 0 0))
                               (faz-jogada (faz-pos 1 3)(faz-pos 1 1)))))
                       (damas-havaianas)
                       (em controle 'humano-escolheu-cor 'b)
                       (em controle 'primeiras-pecas (faz-pos 0 1) (faz-pos 0 0))
                       (em controle 'recebe-jogada-humano 
                           (faz-jogada (faz-pos 2 1)(faz-pos 0 1)))
                       (and
                        (= dh-inicio-c dh-pede-primeiras-pecas-c 1)
                        (= dh-executa-jogada-c 3)
                        (= dh-aceita-jogada-c 2)
                        (= (length dh-executa-jogada-jogada) 3)
                        (equal? (first dh-executa-jogada-caps) (list (faz-pos 1 0)))
                        (equal? (first dh-executa-jogada-jogada) 
                                (faz-jogada (faz-pos 2 0) (faz-pos 0 0)))
                        (equal? (second dh-executa-jogada-caps) (list (faz-pos 1 1)))
                        (equal? (second dh-executa-jogada-jogada) 
                                (faz-jogada (faz-pos 2 1)(faz-pos 0 1)))
                        (equal? (third dh-executa-jogada-caps) (list (faz-pos 1 2)))
                        (equal? (third dh-executa-jogada-jogada) 
                                (faz-jogada (faz-pos 1 3)(faz-pos 1 1)))
                        
                        (contadores-a-zero? 
                         (lset-difference eq? contadores 
                                          '(dh-inicio-c
                                            dh-pede-primeiras-pecas-c
                                            dh-aceita-jogada-c
                                            dh-executa-jogada-c
                                            dh-executa-jogada-caps
                                            dh-executa-jogada-jogada)))
                        )) #f)
   
   (cria-teste "teste controle 07"
               '(begin (reset-contadores!)
                       (damas-havaianas)
                       (em controle 'humano-escolheu-cor 'p)
                       (em controle 'primeiras-pecas (faz-pos 0 0) (faz-pos 0 1))
                       (em controle 'recebe-jogada-humano 
                           (faz-jogada (faz-pos 4 0) (faz-pos 0 0)))
                       (and
                        (= dh-inicio-c dh-aceita-jogada-c 
                           dh-assinala-erro-c dh-pede-primeiras-pecas-c 1)
                        (eq? dh-assinala-erro-jog 'humano)
                        (equal? dh-assinala-erro-jogada 
                                (faz-jogada (faz-pos 4 0) (faz-pos 0 0)))
                        (contadores-a-zero? 
                         (lset-difference eq? contadores 
                                          '(dh-inicio-c
                                            dh-pede-primeiras-pecas-c
                                            dh-aceita-jogada-c
                                            dh-assinala-erro-c
                                            dh-assinala-erro-jog
                                            dh-assinala-erro-jogada)))
                        )) #f)
   
   (cria-teste "teste controle 08"
               '(begin (reset-contadores!)
                       (set-jogador-lambda! (cria-jogador-jogada 
                                             (list (faz-jogada (faz-pos 3 1)(faz-pos 0 1)))))
                       (damas-havaianas)
                       (em controle 'humano-escolheu-cor 'p)
                       (em controle 'primeiras-pecas (faz-pos 0 0) (faz-pos 0 1))
                       (em controle 'recebe-jogada-humano 
                           (faz-jogada (faz-pos 2 0) (faz-pos 0 0)))
                       (and
                        (= dh-inicio-c dh-pede-primeiras-pecas-c dh-aceita-jogada-c 
                           dh-executa-jogada-c dh-assinala-erro-c 1)
                        (equal? (first dh-executa-jogada-jogada) 
                                (faz-jogada (faz-pos 2 0) (faz-pos 0 0)))
                        (equal? (first dh-executa-jogada-caps) (list (faz-pos 1 0)))
                        (eq? dh-assinala-erro-jog 'automatico)
                        (equal? dh-assinala-erro-jogada 
                                (faz-jogada (faz-pos 3 1)(faz-pos 0 1)))
                        (contadores-a-zero? 
                         (lset-difference eq? contadores 
                                          '(dh-inicio-c
                                            dh-pede-primeiras-pecas-c
                                            dh-aceita-jogada-c
                                            dh-executa-jogada-c
                                            dh-executa-jogada-jogada
                                            dh-executa-jogada-caps
                                            dh-assinala-erro-c
                                            dh-assinala-erro-jog
                                            dh-assinala-erro-jogada)))
                        )) #f)
   
   (cria-teste "teste controle 09"
               '(begin (reset-contadores!)
                       (damas-havaianas)
                       (em controle 'humano-escolheu-cor 'p)
                       (em controle 'primeiras-pecas (faz-pos 0 0) (faz-pos 0 1))
                       (em controle 'recebe-jogada-humano 
                           (faz-jogada (faz-pos 0 0) (faz-pos 0 0)))
                       (and
                        (= dh-inicio-c dh-aceita-jogada-c 
                           dh-termina-jogo-c dh-pede-primeiras-pecas-c 1)
                        (eq? dh-termina-jogo-venc 'automatico)
                        (contadores-a-zero? 
                         (lset-difference eq? contadores 
                                          '(dh-inicio-c
                                            dh-pede-primeiras-pecas-c
                                            dh-aceita-jogada-c
                                            dh-termina-jogo-c
                                            dh-termina-jogo-venc)))
                        ))  #f)
   )) 

(define (testa testes)
  (dolist (teste testes)
          (let ((res (no-errors (eval (forma-teste teste)))))
            (newline)
            (display (nome-teste teste))
            (cond ((and (eq? res #f) (erro?-teste teste)) (display " - Passou."))
                  ((eq? res #f) (display " - FALHOU."))
                  (else (display " - Passou."))))))



(testa *ts*)

