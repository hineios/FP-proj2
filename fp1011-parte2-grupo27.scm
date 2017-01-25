; --------------------------------------
; Programa: Damas Havaianas
; --------------------------------------
;
; Descrição: Joga contra um Humano ou contra outro
;            Jogador Automático.
;
; Criado por: Ricardo Leitão (69632)
;             Diogo Andrade (70031)
;             Fábio Almeida (70227)
;
; Grupo Nº:   27
;
; Data de criação: 19 de Dezembro de 2010?
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    PARTE 1    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI posicao ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Uma posicao é constituída por uma linha e uma coluna,  ;;;;;;;;;;;;;
;;;;;;;; ambas inteiros entre 0 e 7. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REP.INTERNA: Uma posicao é representada por um par,   ;;;;;;;;;;;;;;
;;;;;;;; cujo primeiro elemento contém a linha, e   ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; cujo segundo elemento contém a coluna  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Definição abstracta das operações no enunciado ;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;  Operações básicas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Construtor 
;;; 
;;; faz-pos : inteiro x inteiro -> posição 
;;; faz-pos(l, c) devolve a posição correspondente à linha l, coluna c.
(define (faz-pos l c)
  (if (and (inteiro-entre?  0 7 l) (inteiro-entre?  0 7 c))
      (cons l c)
      (error "faz-pos: os argumentos devem ser inteiros entre 0 e 7.")))

;;; Selectores 
;;; 
;;; linha-pos : posição -> inteiro 
;;; linha-pos(p) devolve a linha da posição p. 
(define linha-pos car)

;;; coluna-pos : posição -> inteiro 
;;; coluna-pos(p) devolve a coluna da posição p. 
(define coluna-pos cdr)

;;; Reconhecedor 
;;; 
;;; pos? : universal -> lógico 
;;; pos?(arg) tem o valor verdadeiro, se arg e uma posição, e tem o 
;;; valor falso, em caso contrario. 
(define (pos? x)
  (and (pair? x)
       (inteiro-entre?  0 7 (linha-pos x))
       (inteiro-entre?  0 7 (coluna-pos x))))

;;; procedimento auxiliar
;;;
;;; inteiro-entre? : inteiro x inteiro x inteiro -> lógico
;;; inteiro-entre?(n1, n2, n) devolve verdadeiro se n for um inteiro,
;;; e n1 <= n <= n2
(define (inteiro-entre? n1 n2 n)
  (and (integer? n) (<= n1 n) (<= n n2)))

;;; Teste 
;;; 
;;; pos=? : posição x posição -> lógico 
;;; pos=?(p1 , p2) tem o valor verdadeiro, se p1 e p2 sao posições 
;;; iguais, e tem o valor falso, em caso contrario. 
(define (pos=? pos1 pos2)
  (and (= (linha-pos pos1) (linha-pos pos2))
       (= (coluna-pos pos1) (coluna-pos pos2))))


;;;;;;;;;;;;;;;;;;;;;;;  Operações de alto nível ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; distancia : posição x posição -> real 
;;; distancia(p1, p2) devolve a distância entre as posições p1 e p2. 
;;; Se p1 = (l1, c1) e p2 = (l2, c2), então distancia(p1, p2) = 
;;; sqrt((l1 - l2)**2 + (c1 - c2)**2).
(define (distancia pos1 pos2)
  (sqrt (+ (expt (- (linha-pos pos1) (linha-pos pos2)) 2)
           (expt (- (coluna-pos pos1) (coluna-pos pos2)) 2))))

;;; mesma-direccao=? : posição x posição -> lógico 
;;; mesma-direccao=?(p1 , p2) tem o valor verdadeiro, se p1 e p2 estão
;;; na mesma linha, ou na mesma coluna, e tem o valor falso, 
;;; em caso contrário.  
(define (mesma-direccao? pos1 pos2)
  (or (= (linha-pos pos1) (linha-pos pos2)) 
      (= (coluna-pos pos1) (coluna-pos pos2))))

;;; adjacentes? : posição x posição -> lógico 
;;; adjacentes?(p1 , p2) tem o valor verdadeiro, se p1 e p2 são 
;;; posições adjacentes, e tem o valor falso, em caso contrário. 
(define (adjacentes? pos1 pos2)
  (and (mesma-direccao? pos1 pos2)
       (= 1 (distancia pos1 pos2))))

;;; adjacentes : posição -> lista de posições 
;;; adjacentes(p) devolve uma lista cujos elementos são as posições 
;;; adjacentes à posição p. Por exemplo, adjacentes(faz-pos(0, 0)) 
;;; devolve a lista ((1, 0)(0, 1)), e adjacentes(faz-pos(1, 2) 
;;; devolve a lista ((2, 2)(0, 2)(1, 3)(1, 1)).
(define (adjacentes pos)
  (let* ((l (linha-pos pos))
         (c (coluna-pos pos))
         (tentativas (list (cons (+ l 1) c)
                           (cons (- l 1) c)
                           (cons l (+ c 1))
                           (cons l (- c 1)))))
    (transforma (lambda (par)
                  (faz-pos (car par) (cdr par)))
                (filtra (lambda (tent)
                          (and (inteiro-entre?  0 7 (car tent))
                               (inteiro-entre?  0 7 (cdr tent))))
                        tentativas))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI jogada ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Uma jogada é constituída por duas posições,  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; o início e o fim da jogada. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REP.INTERNA: Uma jogada é representada por um par,   ;;;;;;;;;;;;;;
;;;;;;;; cujo primeiro elemento contém o início da jogada, e   ;;;;;;;;;;;;;
;;;;;;;; cujo segundo elemento contém o fim da jogada  ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Definição abstracta das operações no enunciado ;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;  Operações básicas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Construtor 
;;; 
;;; faz-jogada : posição x posição -> jogada 
;;; faz-jogada(p1, p2) devolve uma jogada com início em p1 e fim em p2. 
(define (faz-jogada pos1 pos2)
  (if (and (pos? pos1) (pos? pos2))
      (cons pos1 pos2)
      (error "faz-jogada: os argumentos devem ser posições.")))

;;; Selectores 
;;; 
;;; inicio-jogada : jogada -> posição 
;;; inicio-jogada(j) devolve o início da jogada j.
(define inicio-jogada car)

;;; fim-jogada : jogada -> posição 
;;; fim-jogada(j) devolve o fim da jogada j.
(define fim-jogada cdr)

;;; Reconhecedores 
;;; 
;;; jogada? : universal -> lógico 
;;; jogada?(arg) tem o valor verdadeiro, se arg e uma jogada, e tem o 
;;; valor falso, em caso contrario. 
(define (jogada? x)
  (and (pair? x)
       (pos? (car x))
       (pos? (cdr x))))

;;; jogada-nula? : jogada -> lógico 
;;; jogada-nula?(arg) tem o valor verdadeiro, se arg e uma jogada nula,
;;; e tem o valor falso, em caso contrario. 
(define (jogada-nula? j)
  (pos=? (inicio-jogada j) (fim-jogada j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI posicoes-de-jogada ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Um elemento do tipo posicoes-de-jogada é constituído  ;;;;;;;;;;;;;;
;;;;;;;; por duas listas de posições; uma destas listas contém as posições ;;
;;;;;;;; que devem conter peças do adversário que serão capturadas,;;;;;;;;;;
;;;;;;;; e a outra contém as posições que devem estar livres. ;;;;;;;;;;;;;;;
;;;;;;;; REP.INTERNA: Um elemento do tipo posicoes-de-jogada é representado ;
;;;;;;;; por um par cujo primeiro elemento contém a lista de posições de ;;;;
;;;;;;;; peças capturadas, e cujo segundo elemento contém;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  a lista de posições livres  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Definição abstracta das operações no enunciado ;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;  Operações básicas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Construtor 
;;; 
;;; faz-posicoes-de-jogada : lista de posições x lista de posições -> posições de jogada
;;; faz-posicoes-de-jogada(pc, pl) devolve o elemento do tipo posições 
;;; de jogada cujas posições de peças capturadas são as da lista pc, e
;;; cujas posições livres são as da lista pl. 
(define (faz-posicoes-de-jogada pecas-capturadas posicoes-livres)
  (if (or (not (lista? pecas-capturadas)) (not (lista? posicoes-livres))
          (not (todos-satisfazem? pos? pecas-capturadas))
          (not (todos-satisfazem? pos? posicoes-livres)))
      (error "faz-posicoes-de-jogada: os argumentos devem listas de posicoes")
      (cons pecas-capturadas posicoes-livres)))

;;; Selectores 
;;; 
;;; pecas-capturadas : posições de jogada -> lista de posições 
;;; pecas-capturadas(pj) devolve a lista de posições de peças capturadas 
;;; de pj.
(define pecas-capturadas car)

;;; resposta-vermelhos : posições de jogada -> lista de posições 
;;; resposta-vermelhos(r) devolve a lista de posições livres de pj.
(define posicoes-livres cdr)

;;;;;;;;;;;;;;;;;;;;;;;  Operação de alto nível ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; posicoes-de-jogada : jogada -> posições de jogada 
;;; posicoes-de-jogada(j) devolve as posições de jogada da jogada j.
(define (posicoes-de-jogada jogada)
  
  (define (posicoes-de-jogada-aux jogada)
    (let* ((pos1 (inicio-jogada jogada))
           (pos2 (fim-jogada jogada))
           (l1 (linha-pos pos1))
           (c1 (coluna-pos pos1))
           (l2 (linha-pos pos2))
           (c2 (coluna-pos pos2)))
      (if (= l1 l2)
          (faz-posicoes-de-jogada
           (transforma (lambda (col) (faz-pos l1 col))
                       (pecas c1 c2))
           (transforma (lambda (col) (faz-pos l1 col))
                       (livres c1 c2)))
          (faz-posicoes-de-jogada
           (transforma (lambda (linha) (faz-pos linha c1))
                       (pecas l1 l2))
           (transforma (lambda (linha) (faz-pos linha c1))
                       (livres l1 l2))))))
  
  (if (or (not (jogada? jogada))
          (not (mesma-direccao? (inicio-jogada jogada) (fim-jogada jogada)))
          (not (even? (distancia (inicio-jogada jogada) (fim-jogada jogada)))))
      (nova-lista)
      (posicoes-de-jogada-aux jogada)))

;;; de-2-em-2: inteiro x inteiro -> lista de inteiros
;;; de-2-em-2(lim-inf, lim-sup) devolve a lista cujos elementos são
;;; lim-inf, lim-inf + 2, lim-inf + 4, ..., lim-sup, 
;;; se (lim-sup - lim-inf) for par 
;;; ou lim-inf, lim-inf + 2, lim-inf + 4, ..., lim-sup - 1, 
;;; se (lim-sup - lim-inf) for ímpar.
(define (de-2-em-2 lim-inf lim-sup)
  (if (> lim-inf lim-sup)
      (nova-lista)
      (insere lim-inf
              (de-2-em-2 (+ 2 lim-inf)
                         lim-sup))))

;;; pecas: inteiro x inteiro -> lista de inteiros
;;; pecas(inicio, fim) devolve a lista com as linhas/colunas que devem 
;;; conter peças numa jogada da linha/coluna inicio para a linha/coluna
;;; fim.
(define (pecas inicio fim)  
  (de-2-em-2 (add1 (min inicio fim)) (max inicio fim)))

;;; livres: inteiro x inteiro -> lista de inteiros
;;; livres(inicio, fim) devolve a lista com as linhas/colunas que devem
;;; estar livres numa jogada da linha/coluna inicio para a linha/coluna
;;; fim.
(define (livres inicio fim)  
  (if (> fim inicio)
      (de-2-em-2 (+ 2  inicio) fim)
      (de-2-em-2 fim (- inicio 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI LISTA ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;   Definido no livro ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Construtores
;;; 
;;; nova-lista : {} -> lista 
;;; nova-lista() tem como valor uma lista sem elementos. 
(define (nova-lista)
  ())

;;; insere : elemento x lista -> lista 
;;; insere(elem, lst) tem como valor a lista que resulta de inserir o 
;;; elemento elem na primeira posicao da lista lst. 
(define (insere el lst)
  (cons el lst))

;;; Selectores
;;; 
;;; primeiro : lista -> elemento 
;;; primeiro(lst) tem como valor o elemento que se encontra na primeira
;;; posicao da lista lst. Se a lista nao tiver elementos, o valor desta
;;; operacao e' indefinido. 
(define (primeiro lst)
  (if (null? lst)
      (error "primeiro: a lista não tem  elementos")
      (car lst)))

;;; resto : lista -> lista 
;;; resto(lst) tem como valor a lista que resulta de remover o primeiro
;;; elemento da lista lst. Se a lista nao tiver elementos, o valor 
;;; desta operacao  e' indefinido.
(define (resto lst)
  (if (null? lst)
      (error "resto: a lista não tem  elementos")
      (cdr lst)))

;;; Reconhecedores
;;; 
;;; lista? : universal -> logico 
;;; lista?(arg) tem o valor verdadeiro se arg  e' uma lista e tem o valor
;;; falso em caso contrario. 
(define (lista? x)
  (cond ((null? x) #t)
        ((pair? x) (lista? (cdr x)))
        (else #f)))

;;; lista-vazia? : lista -> logico 
;;; lista-vazia?(lst) tem o valor verdadeiro se a lista lst  e' a lista 
;;; vazia e tem o valor falso em caso contrario. 
(define (lista-vazia? lst)
  (null? lst))

;;; Testes
;;; 
;;; listas=? : lista x lista x predicado -> logico 
;;; listas=?(lst1, lst2, pred) tem o valor verdadeiro se a lista lst1 e'
;;; igual a lista lst2, comparando os seus elementos com pred, e tem o 
;;; valor falso em caso contrario. 
(define (listas=? lst1 lst2 elem=?)
  (cond ((null? lst1) (null? lst2))
        ((null? lst2) #f)
        ((elem=? (car lst1) (car lst2)) 
         (listas=? (cdr lst1) (cdr lst2) elem=?))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCIONAIS SOBRE LISTAS;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;   filtra  (definido no livro) 
;;; filtra : predicado x lista -> lista 
;;; filtra(p, lst) devolve uma lista com todos os elementos da lista lst
;;; para os quais a aplicação do predicado p retorna o valor lógico #t. 
(define (filtra tst? lst)
  (cond ((lista-vazia? lst) lst)
        ((tst? (primeiro lst))
         (insere (primeiro lst)
                 (filtra tst? (resto lst))))
        (else (filtra tst? (resto lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;   transforma  (definido no livro) 
;;; transforma : procedimento x lista -> lista 
;;; transforma(p, lst) devolve uma lista com os resultados da aplicação do
;;; procedimento p a cada um dos elementos de lst. 
(define (transforma tr lst)
  (if (lista-vazia? lst) 
      lst
      (insere (tr (primeiro lst))
              (transforma tr (resto lst)))))

;;; todos-satisfazem? : predicado x lista -> lógico 
;;; todos-satisfazem?(p, lst) devolve verdadeiro sse todos os elementos da
;;; lista lst satisfizerem o predicado p.
(define (todos-satisfazem? tst? lst)
  (cond ((lista-vazia? lst) #t)
        ((tst? (primeiro lst))
         (todos-satisfazem? tst? (resto lst)))
        (else #f)))

(define (inverte lst)
  (define (inverte-aux lst1 lst2)
    (if (lista-vazia? lst1)
        lst2
        (inverte-aux (resto lst1)
                     (insere (primeiro lst1) lst2))))
  (inverte-aux lst (nova-lista)))

(define (comprimento lst)
  (if (lista-vazia? lst)
      0
      (+ 1 (comprimento (resto lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    PARTE 2    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---------------------------------------------------
; [=] Definição Tipo:  Tabuleiro
;---------------------------------------------------

;--------------------------------------------------
; [»] Operações Básicas
;--------------------------------------------------


;============== Construtor =============

(define (tabuleiro-inicial)
  (define (preenche tab inicio fim)
    (if (= inicio fim)
        (vector-set! tab inicio (vectores-b))
        (begin
          (if (even? inicio)
              (vector-set! tab inicio (vectores-p))
              (vector-set! tab inicio (vectores-b)))
          (preenche tab (add1 inicio) fim))))
  (define (vectores-b)
    (vector 'b 'p 'b 'p 'b 'p 'b 'p))
  (define (vectores-p)
    (vector 'p 'b 'p 'b 'p 'b 'p 'b))
  (let ((tab (make-vector 8)))
    (preenche tab 0 7)
    tab))

;================ Selector =================
(define (conteudo-tabuleiro t p)
  (vector-ref (vector-ref t (linha-pos p)) (coluna-pos p)))

;================ Modificadores ============
(define (coloca-peca! t p c)
  (vector-set! (vector-ref t (linha-pos p)) (coluna-pos p) c))
(define (retira-peca! t p)
  (vector-set! (vector-ref t (linha-pos p)) (coluna-pos p) 'v))


;-----------------------------------------------
;        Código Predefinido no Enunciado
;-----------------------------------------------

(define (em proc mens . args)
  (apply (proc mens) args))

(define controle null)

(define (damas-havaianas) 
  (set! controle (cria-controle))
  (eval (quote (dh-inicio))))

(require "interface-damas.zo")



(define jogador null)

(define (cria-jogador27 cor-pec)
  (if (eq? cor-pec 'b)
      (begin (set! jogador (jogador27))
             (jogador27))
      (begin (set! jogador (jogador27))
             (jogador27))))


;-----------------------------------------------------------
; [8] Definição Programa: Controle
;-----------------------------------------------------------

(define (cria-controle)
  (let ((tabuleiro (tabuleiro-inicial)) ;as variáveis usadas no let servem para que o programa de controle as utilize para avaliar jogadas.
        (cor-humano null)
        (cor-automatico null))
    
    (define (humano-escolheu-cor cor)
      (display "controle: mensagem humano-escolheu-cor ")
      (display cor)
      (newline)    
      (if (eq? cor 'p)
          (begin
            (set! cor-humano 'p)
            (set! cor-automatico 'b))
          (begin
            (set! cor-humano 'b)
            (set! cor-automatico 'p)))
      (eval (cria-jogador27 cor-automatico))
      (eval (dh-pede-primeiras-pecas jogador)))
    
    (define (primeiras-pecas pos-humano pos-automatico)
      (display "controle: mensagem primeiras-pecas ")
      (display pos-humano)
      (display " ")
      (display pos-automatico)
      (newline)
      (retira-peca! tabuleiro pos-humano) ;aqui é actualizado o tabuleiro do controle com a primeira peça tirada pelo jogador humano
      (retira-peca! tabuleiro pos-automatico) ;aqui é actualizado o tabuleiro do controle com a primeira peça tirada pelo jogador automático
      (if (eq? cor-automatico 'p) ;este if corre, de acordo com a côr do jogador automático, a primeira jogada do jogo
          (recebe-jogada-automatico (em jogador 'joga))
          (eval (dh-aceita-jogada))))
    
    
    
    (define (recebe-jogada-humano jogada)      
      (define (jogada-valida-hum? jogada);este procedimento irá receber uma jogada e verificar se ele é válido (decidimos fazer dois idênticos, um para avaliar as jogadas do jogador humano e outro para avaliar as jogadas do jogador automático)
        (define (posicao-b? pos); verifica o conteúdo de uma posição e devolve #t se for branca e #f se não for.
          (eq? (conteudo-tabuleiro tabuleiro pos) 'b))
        (define (posicao-p? pos); verifica o conteúdo de uma posição e devolve #t se for preta e #f se não for.
          (eq? (conteudo-tabuleiro tabuleiro pos) 'p))
        (define (posicao-v? pos); verifica o conteúdo de uma posição e devolve #t se for vazia e #f se não for.
          (eq? (conteudo-tabuleiro tabuleiro pos) 'v))
        ;para verificar a validade de uma jogada é necessário essa jogada satisfazer várias condições:
        (and (not (adjacentes? (inicio-jogada jogada)(fim-jogada jogada))) ;primeiro que tudo o inicio e o fim da jogada não podem ser posições adjacentes
             (mesma-direccao? (inicio-jogada jogada)(fim-jogada jogada));depois o inicio e o fim da jogada têm de estar na mesma coluna ou na mesma linha
             (even? (distancia (inicio-jogada jogada) (fim-jogada jogada))); depois a distância entre o início e o fim da jogada tem de ser par
             (todos-satisfazem? posicao-v? (posicoes-livres (posicoes-de-jogada jogada))); é necessário verificar se as posições livres da jogada estão mesmo vazias
             (if (eq? cor-humano 'p);dependendo da cor das peças do jogador humano
                 (posicao-p? (inicio-jogada jogada)); a posição correspondente ao início da jogada tem de ser preta
                 (posicao-b? (inicio-jogada jogada))); ou branca
             (if (eq? cor-humano 'p);dependendo também da cor das peças do jogador humano
                 (todos-satisfazem? posicao-b? (pecas-capturadas (posicoes-de-jogada jogada)));as posições correspondentes às peças capturadas da jogada têm de ser brancas
                 (todos-satisfazem? posicao-p? (pecas-capturadas (posicoes-de-jogada jogada))))));ou pretas
      ;se todas estas condições se verificarem uma jogada é considerada válida
      (define (tira-humano-jogou jog)
        (define (aux lista)
          (if (= (comprimento lista) 1)
              (retira-peca! tabuleiro (primeiro lista))
              (begin (retira-peca! tabuleiro (primeiro lista))
                     (aux (resto lista)))))
        (coloca-peca! tabuleiro 
                      (fim-jogada jog) 
                      (conteudo-tabuleiro tabuleiro (inicio-jogada jog)))
        (retira-peca! tabuleiro (inicio-jogada jog))
        (aux (pecas-capturadas (posicoes-de-jogada jog))))      
      (display "controle: mensagem recebe-jogada-humano ")
      (display jogada)
      (newline)
      (cond ; depois de avaliada a jogada do jogador humano este cond faz a jogada actualizando a interface e o tabuleiro do controle. Caso a jogada não seja válida é assinalado o erro e dada a vitória ao jogador automático. Caso a jogada seja nula, é terminado o jogo e dada a vitória ao jogador automático.
        ((jogada-nula? jogada) (eval (dh-termina-jogo 'automatico)))
        ((jogada-valida-hum? jogada)
         (dh-executa-jogada jogada (pecas-capturadas (posicoes-de-jogada jogada)))
         (tira-humano-jogou jogada)
         (em jogador 'humano-jogou jogada)
         (recebe-jogada-automatico (em jogador 'joga)))
        (else (eval (dh-assinala-erro 'humano jogada)))))
    
    
    (define (recebe-jogada-automatico jogada)
      (define (automatico-jogou jog)
        (define (aux lista)
          (if (= (comprimento lista) 1)
              (retira-peca! tabuleiro (primeiro lista))
              (begin (retira-peca! tabuleiro (primeiro lista))
                     (aux (resto lista)))))
        (coloca-peca! tabuleiro 
                      (fim-jogada jog) 
                      (conteudo-tabuleiro tabuleiro (inicio-jogada jog)))
        (retira-peca! tabuleiro (inicio-jogada jog))
        (aux (pecas-capturadas (posicoes-de-jogada jog))))
      (define (jogada-valida-auto? jogada);este procedimento é em todos os aspectos igual ao jogada-valida-hum? exceptuando na cor utilizada (que neste caso é a cor do jogador automatico
        (define (posicao-b? pos)
          (eq? (conteudo-tabuleiro tabuleiro pos) 'b))
        (define (posicao-p? pos)
          (eq? (conteudo-tabuleiro tabuleiro pos) 'p))
        (define (posicao-v? pos)
          (eq? (conteudo-tabuleiro tabuleiro pos) 'v))
        
        (and (not (adjacentes? (inicio-jogada jogada) (fim-jogada jogada)))
             (mesma-direccao? (inicio-jogada jogada) (fim-jogada jogada))
             (even? (distancia (inicio-jogada jogada) (fim-jogada jogada)))
             (todos-satisfazem? posicao-v? (posicoes-livres (posicoes-de-jogada jogada)))
             (if (eq? cor-automatico 'p)
                 (eq? (conteudo-tabuleiro tabuleiro (inicio-jogada jogada)) 'p)
                 (eq? (conteudo-tabuleiro tabuleiro (inicio-jogada jogada)) 'b))
             (if (eq? cor-automatico 'p)
                 (todos-satisfazem? posicao-b? (pecas-capturadas (posicoes-de-jogada jogada)))
                 (todos-satisfazem? posicao-p? (pecas-capturadas (posicoes-de-jogada jogada))))))
      (cond ; depois de avaliada a jogada do jogador automático este cond faz a jogada actualizando a interface e o tabuleiro do controle. Caso a jogada não seja válida é assinalado o erro e dada a vitória ao jogador humano. Caso a jogada seja nula, é terminado o jogo e dada a vitória ao jogador humano.
        ((jogada-nula? jogada) (eval (dh-termina-jogo 'humano)))
        ((jogada-valida-auto? jogada)
         (dh-executa-jogada jogada (pecas-capturadas (posicoes-de-jogada jogada)))
         (automatico-jogou jogada)
         (eval (dh-aceita-jogada)))
        (else (eval (dh-assinala-erro 'automatico jogada)))))
    
    
    (lambda (m)
      (case m
        ((humano-escolheu-cor) humano-escolheu-cor)
        ((primeiras-pecas) primeiras-pecas)
        ((recebe-jogada-humano) recebe-jogada-humano)))))




;----------------------------------------------------------------------
; [8] Definição Programa: Jogador
;----------------------------------------------------------------------

(define (jogador27)
  (let ((tabuleiro (tabuleiro-inicial))
        (cor-pec 'v))
    (define (joga)  ;o Procedimento joga é um procedimento bastante complexo constituído por vários algoritmos diferentes
      (let ((jogada null))
        (define (encontra-pos-vazia tab) ;o "encontra-poz-vazia" irá percorrer o tabuleiro (definido como estado interno do jogador automatico) de modo a encontrar as posições cujo conteudo é vazio (e potenciais posições para o final de uma jogada). As posições são percorridas começando na posição (0 . 0) seguindo ao longo da linha e quando chega ao final da linha, passa para a proxima até chegar à posição (7 .7).
          (define (aux pos lista)
            (cond ((pos=? pos (faz-pos 7 7))
                   (if (eq? (conteudo-tabuleiro tab pos) 'v)
                       (insere pos lista)
                       lista))
                  (else (if (eq? (conteudo-tabuleiro tab pos) 'v)
                            (aux (proxima-pos pos) (insere pos lista))
                            (aux (proxima-pos pos) lista)))))
          (inverte (aux (faz-pos 0 0) (nova-lista))))
        (define (proxima-pos pos) ;procedimento que auxilia o "encontra-pos-vazia" devolvendo-lhe a posição seguinte à posição apresentada.
          (cond ((and (= (linha-pos pos) 7)
                      (= (coluna-pos pos) 7))
                 (void))
                ((= (coluna-pos pos) 7)
                 (faz-pos (add1 (linha-pos pos)) 0))
                (else (faz-pos (linha-pos pos)
                               (add1 (coluna-pos pos))))))
        (define (posicao-b? pos) ;averigua se o conteudo da posição fornecida é b, ou seja devolve verdadeiro se a peça nessa posição for branca
          (eq? (conteudo-tabuleiro tabuleiro pos) 'b))
        (define (posicao-p? pos) ;identico ao anterior , mas para as peças pretas
          (eq? (conteudo-tabuleiro tabuleiro pos) 'p))
        (define (duas-antes-linha pos) ;devolve a posição que está duas posições atras da fornecida ao longo da linha, ou seja, a avaliação da expressão (duas-antes-linha '(3 . 3) devolve a posição (3 . 1).
          (faz-pos (linha-pos pos)
                   (- (coluna-pos pos) 2)))
        (define (duas-depois-linha pos) ;devolve a posição que está duas posições depois da fornecida ao longo da linha, ou seja, a avaliação da expressão (duas-depois-linha '(3 . 3) devolve a posição (3 . 5).
          (faz-pos (linha-pos pos)
                   (+ (coluna-pos pos) 2)))
        (define (duas-antes-coluna pos) ;devolve a posição que está duas posições atras da fornecida ao longo da coluna, ou seja, a avaliação da expressão (duas-antes-coluna '(3 . 3) devolve a posição (1 . 3).
          (faz-pos (- (linha-pos pos) 2)
                   (coluna-pos pos)))
        (define (duas-depois-coluna pos) ;devolve a posição que está duas posições depois da fornecida ao longo da coluna, ou seja, a avaliação da expressão (duas-depois-coluna '(3 . 3) devolve a posição (5 . 3).
          (faz-pos (+ (linha-pos pos) 2)
                   (coluna-pos pos)))
        (define (limpa-adjacentes lista) ;este procecimento recebe uma lista de posições (em todos os casos que é utilizado tratam-se de listas de posições adjacentes, daí o nome) e vai descartar todas as posições que forem da cor das peças do jogador automatico. 
          (if (eq? cor-pec 'p)
              (filtra posicao-b? lista)
              (filtra posicao-p? lista)))
        (define (direccao1 pos-vazia pos);este procedimento recebe duas posições adjacentes e devolve a posição que está no seguimento da primeira com a segunda, por exemplo: a avaliação da expressão (direccão1 '(0 . 0) '(0 . 1)) devolve a posição (0 . 2). A divisão deste procedimento em 9 partes diferentes está relaccionado com a divisão do tabuleiro em 9 partes, divisão esta explícita no relatório. (o nome dos argumentos pos-vazia e pos deve-se ao facto da utilização do procedimento ser sempre com uma posição com conteudo 'v e uma posição adjacente a esta.
          (cond ((> 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                 (duas-depois-coluna pos-vazia))
                ((> 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                 (duas-depois-linha pos-vazia))
                (else 'nao)))
        (define (direccao2 pos-vazia pos)
          (if (= (linha-pos pos-vazia) (linha-pos pos))
              (if (< 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                  (duas-antes-linha pos-vazia)
                  (duas-depois-linha pos-vazia))
              (if (> 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                  (duas-depois-coluna pos-vazia)
                  'nao)))
        (define (direccao3 pos-vazia pos)
          (cond ((< 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                 (duas-antes-linha pos-vazia))
                ((> 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                 (duas-depois-coluna pos-vazia))
                (else 'nao)))
        (define (direccao4 pos-vazia pos)
          (if (= (coluna-pos pos-vazia) (coluna-pos pos))
              (if (< 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                  (duas-antes-coluna pos-vazia)
                  (duas-depois-coluna pos-vazia))
              (if (> 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                  (duas-depois-linha pos-vazia)
                  'nao)))
        (define (direccao5 pos-vazia pos)
          (if (= (linha-pos pos-vazia) (linha-pos pos))
              (if (< 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                  (duas-antes-linha pos-vazia)
                  (duas-depois-linha pos-vazia))
              (if (< 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                  (duas-antes-coluna pos-vazia)
                  (duas-depois-coluna pos-vazia))))
        (define (direccao6 pos-vazia pos)
          (if (= (coluna-pos pos-vazia) (coluna-pos pos))
              (if (< 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                  (duas-antes-coluna pos-vazia)
                  (duas-depois-coluna pos-vazia))
              (if (< 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                  (duas-antes-linha pos-vazia)
                  'nao)))
        (define (direccao7 pos-vazia pos)
          (cond ((< 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                 (duas-antes-coluna pos-vazia))
                ((> 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                 (duas-depois-linha pos-vazia))
                (else 'nao)))
        (define (direccao8 pos-vazia pos)
          (if (= (linha-pos pos-vazia) (linha-pos pos))
              (if (< 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                  (duas-antes-linha pos-vazia)
                  (duas-depois-linha pos-vazia))
              (if (< 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                  (duas-antes-coluna pos-vazia)
                  'nao)))
        (define (direccao9 pos-vazia pos)
          (cond ((< 0 (- (coluna-pos pos-vazia) (coluna-pos pos)))
                 (duas-antes-linha pos-vazia))
                ((< 0 (- (linha-pos pos-vazia) (linha-pos pos)))
                 (duas-antes-coluna pos-vazia))
                (else 'nao)))
        (define (automatico-jogou jog);procedimento semelhante ao humano-jogou descrito mais a baixo
          (define (aux lista)
            (if (= (comprimento lista) 1)
                (retira-peca! tabuleiro (primeiro lista))
                (begin (retira-peca! tabuleiro (primeiro lista))
                       (aux (resto lista)))))
          (coloca-peca! tabuleiro 
                        (fim-jogada jog) 
                        (conteudo-tabuleiro tabuleiro (inicio-jogada jog)))
          (retira-peca! tabuleiro (inicio-jogada jog))
          (aux (pecas-capturadas (posicoes-de-jogada jog))))
        (define (confirma-jogada lista-vazias lista-adjacentes pos-vazia);este procedimento é o que vai verificar se existem jogadas disponiveis ou não no tabuleiro, recebe uma lista de posições vazias, uma lista de posições adjacentes à primeira posição vazia da lista de posições vazias que satisfazem certas especificações (o seu conteúdo tem de ser de cor contrária à cor do jogador automático) e uma posição vazia (primeira posição da lista de posições vazias, ou seja a posição que está a ser avaliada)
          (cond ((lista-vazia? lista-vazias);existem duas condições de paragem na procura de jogadas. caso a lista das posições seja vazia, ou seja não existem jogadas possíveis.
                 (set! jogada (faz-jogada (faz-pos 0 0) (faz-pos 0 0))))
                ((lista-vazia? lista-adjacentes);ou caso a lista das adjacentes seja vazia
                 (if (lista-vazia? (resto lista-vazias));e a lista das posições vazias só tenha um elemento, ou seja, todas as opções já foram analisadas e escluidas
                     (set! jogada (faz-jogada (faz-pos 0 0) (faz-pos 0 0)))
                     (confirma-jogada (resto lista-vazias) ; se, por outro lado a lista das posições vazias não tiver apenas um elemento, isso significa que podem ainda existir jogadas no tabuleiro, por isso chama-se o confirma-jogada recursivamente, eliminando a posição vazia que se estava a avaliar
                                      (limpa-adjacentes (adjacentes (primeiro (resto lista-vazias))))
                                      (primeiro (resto lista-vazias)))))
                ;todos os outros casos não são de paragem                         
                ((and (< -1 (linha-pos pos-vazia) 2) (< -1 (coluna-pos pos-vazia) 2));primeiro que tudo avalia-se a zona do tabuleiro em que a posição vazia está, se a posição vazia se encontrar na zona 1 prosegue-se a avaliação por este ramo.
                 (if (eq? (direccao1 pos-vazia (primeiro lista-adjacentes)) 'nao);depois verifica-se se há ou não uma posição na direcção das duas fornecidas, se nao houver, passa-se à posição seguinte chamando o confirma-jogada novamente.
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao1 pos-vazia (primeiro lista-adjacentes))) cor-pec);se houver verifica-se o conteudo da posição proveniente da avaliação do procedimento direccao1. se esta posição contiver uma peça da cor do jogador automático, encontrámos a nossa jogada
                         (set! jogada (faz-jogada (direccao1 pos-vazia (primeiro lista-adjacentes));por isso temos de actualizar a jogada com a que acabamos de encontrar
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))));caso a posição nao tenha uma peça da cor do jogador automatico, chama-se o confirma-jogada recursivamente para verificar o resto das possibilidades
                
                ((and (< -1 (linha-pos pos-vazia) 2) (< 1 (coluna-pos pos-vazia) 6));todos os outros casos são idênticos ao anterior, apenas divergem na zona do tabuleiro.
                 (if (eq? (direccao2 pos-vazia (primeiro lista-adjacentes)) 'nao)
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao2 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                         (set! jogada (faz-jogada (direccao2 pos-vazia (primeiro lista-adjacentes))
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))))
                
                
                ((and (< -1 (linha-pos pos-vazia) 2) (< 5 (coluna-pos pos-vazia) 8))
                 (if (eq? (direccao3 pos-vazia (primeiro lista-adjacentes)) 'nao)
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao3 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                         (set! jogada (faz-jogada (direccao3 pos-vazia (primeiro lista-adjacentes))
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))))
                
                ((and (< 1 (linha-pos pos-vazia) 6) (< -1 (coluna-pos pos-vazia) 2))
                 (if (eq? (direccao4 pos-vazia (primeiro lista-adjacentes)) 'nao)
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao4 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                         (set! jogada (faz-jogada (direccao4 pos-vazia (primeiro lista-adjacentes))
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))))
                
                ((and (< 1 (linha-pos pos-vazia) 6) (< 1 (coluna-pos pos-vazia) 6))
                 (if (eq? (conteudo-tabuleiro tabuleiro (direccao5 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                     (set! jogada (faz-jogada (direccao5 pos-vazia (primeiro lista-adjacentes))
                                              pos-vazia))
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)))
                
                ((and (< 1 (linha-pos pos-vazia) 6) (< 5 (coluna-pos pos-vazia) 8))
                 (if (eq? (direccao6 pos-vazia (primeiro lista-adjacentes)) 'nao)
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao6 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                         (set! jogada (faz-jogada (direccao6 pos-vazia (primeiro lista-adjacentes))
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))))
                
                ((and (< 5 (linha-pos pos-vazia) 8) (< -1 (coluna-pos pos-vazia) 2))
                 (if (eq? (direccao7 pos-vazia (primeiro lista-adjacentes)) 'nao)
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao7 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                         (set! jogada (faz-jogada (direccao7 pos-vazia (primeiro lista-adjacentes))
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))))
                
                ((and (< 5 (linha-pos pos-vazia) 8) (< 1 (coluna-pos pos-vazia) 6))
                 (if (eq? (direccao8 pos-vazia (primeiro lista-adjacentes)) 'nao)
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao8 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                         (set! jogada (faz-jogada (direccao8 pos-vazia (primeiro lista-adjacentes))
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))))
                
                ((and (< 5 (linha-pos pos-vazia) 8) (< 5 (coluna-pos pos-vazia) 8))
                 (if (eq? (direccao9 pos-vazia (primeiro lista-adjacentes)) 'nao)
                     (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia)
                     (if (eq? (conteudo-tabuleiro tabuleiro (direccao9 pos-vazia (primeiro lista-adjacentes))) cor-pec)
                         (set! jogada (faz-jogada (direccao9 pos-vazia (primeiro lista-adjacentes))
                                                  pos-vazia))
                         (confirma-jogada lista-vazias (resto lista-adjacentes) pos-vazia))))))
        (set! jogada null);é necessário fazer "reset" à jogada antes de se começar a procurar uma nova
        (confirma-jogada (encontra-pos-vazia tabuleiro);aqui chama-se o confirma-jogada para verificar se existem jogadas disponíveis ou não
                         (limpa-adjacentes (adjacentes (primeiro (encontra-pos-vazia tabuleiro))))
                         (primeiro (encontra-pos-vazia tabuleiro)))
        (if (jogada-nula? jogada);depois de avaliado o confirma-jogada verifica-se a jogada (estado interno do procedimento joga)
            jogada;se esta for uma jogada nula apenas a devolve (porque o jogo vai terminar com a derrota do jogador automático)
            (begin (automatico-jogou jogada);se a jogada não for nula, é necessário actualizar o tabuleiro do jogador automatico
                   jogada))));e devolver a jogada
    (define (tira-primeira-peca . pos) 
      (let ((peca-a-tirar (nova-lista)))
        (if (not (lista-vazia? pos)) ;a condição verifica se pos (argumento na forma de lista) não é uma lista vazia. Caso isto de verifique:
            (begin (set! cor-pec 'b) ;aqui actualiza-se o estado interno do jogador automatico alterando destrutivamente o valor de cor-pec para 'b caso exista uma posição como argumento.
                   (cond ((pos=? (car pos) (faz-pos 0 0)) ;será averiguado que posição o jogador humano retirou primeiro.
                          (begin (set! peca-a-tirar
                                       (vector-ref (vector (faz-pos 1 0)(faz-pos 0 1))(random 2))) ;aqui o programa "decide" que peça retirar duma lista das adjacentes à peça tirada pelo jogador humano
                                 (retira-peca! tabuleiro peca-a-tirar) ;depois actualiza o tabuleiro do jogador automatico
                                 peca-a-tirar)) ;e finalmente devolve a peca que é para retirar.
                         ((pos=? (car pos) (faz-pos 3 3))
                          (begin (set! peca-a-tirar
                                       (vector-ref (vector (faz-pos 2 3)(faz-pos 3 2)(faz-pos 4 3)(faz-pos 3 4))(random 4)))
                                 (retira-peca! tabuleiro peca-a-tirar)
                                 peca-a-tirar))
                         ((pos=? (car pos) (faz-pos 4 4))
                          (begin (set! peca-a-tirar
                                       (vector-ref (vector (faz-pos 5 4)(faz-pos 3 4)(faz-pos 4 3)(faz-pos 4 5))(random 4)))
                                 (retira-peca! tabuleiro peca-a-tirar)
                                 peca-a-tirar))
                         ((pos=? (car pos) (faz-pos 7 7))
                          (begin (set! peca-a-tirar
                                       (vector-ref (vector (faz-pos 7 6)(faz-pos 6 7))(random 2)))
                                 (retira-peca! tabuleiro peca-a-tirar)
                                 peca-a-tirar))))
            (begin (set! cor-pec 'p) ;se não houver uma posição como argumento o procedimento actualiza a cor das peças do jogador automatico para 'p
                   (set! peca-a-tirar ;"escolhe" uma peça a tirar 
                         (vector-ref (vector (faz-pos 0 0)(faz-pos 3 3)(faz-pos 4 4)(faz-pos 7 7))(random 4)))
                   (retira-peca! tabuleiro peca-a-tirar) ;actualiza o estado interno do jogador automatico (tabuleiro)
                   peca-a-tirar)))) ;e devolve a peça a tirar
    (define (humano-tirou pos)
      (retira-peca! tabuleiro pos))
    (define (humano-jogou jog) ;procedimento que deve de actualizar o estado interno do jogador automatico (neste caso o tabuleiro). Para isso terá primeiro:
      (define (aux lista) ;este procedimento vai percorrer a lista de posições dada e vai retira-las do tabuleiro.
        (if (= (comprimento lista) 1)
            (retira-peca! tabuleiro (primeiro lista))
            (begin (retira-peca! tabuleiro (primeiro lista))
                   (aux (resto lista)))))
      (coloca-peca! tabuleiro          ;de colocar a peça que está no início da jogada no final da mesma
                    (fim-jogada jog) 
                    (conteudo-tabuleiro tabuleiro (inicio-jogada jog)))
      (retira-peca! tabuleiro (inicio-jogada jog));retirar a peça que está no inicio da jogada
      (aux (pecas-capturadas (posicoes-de-jogada jog)))) ; e finalmente retirar as peças que são comidas durante a jogada através do procedimento auxiliar "aux".
    (lambda (arg)
      (case arg
        ((tira-primeira-peca) tira-primeira-peca)
        ((humano-tirou) humano-tirou)
        ((humano-jogou) humano-jogou)
        ((joga) joga)))))

