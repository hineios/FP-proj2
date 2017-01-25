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
         (todos-satisfazem? tst?(resto lst)))
        (else #f)))
