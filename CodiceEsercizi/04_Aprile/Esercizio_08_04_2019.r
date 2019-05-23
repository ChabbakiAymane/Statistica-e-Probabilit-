#inserisci quali sono le variabili da considerare come successo
v <- 0.22
y <- 0.15

#Indichiamo con p la probabilità di successo mentre con q quella di insuccesso (1-p)
p <- v+y
q <- 1-p
p #1. risposta quesito 1
q+p==1

#2. Probabilità di ottenere x successi su N prove
x <- 1
N <- 18
Pr_esattamente_1 <- choose(N,x)*(p^x)*(q^(N-x))
Pr_esattamente_1

#3. Probabilità di ottenere almeno x successi su N prove
#N.B Al massimo non è propriamente corretto perchè indicherebbe da 1 a x compreso. Abuso di notazione
x <- 5
i <- 0
Pr_almassimo_x <- 0
while(i<x){
    Pr_almassimo_x <- Pr_almassimo_x + choose(N,i)*(p^i)*(q^(N-i))
    i <- i+1
}
Pr_almeno_x <- 1 - Pr_almassimo_x
Pr_almeno_x

#Facciamo una riprova del risultato
i <- x
Pr_almeno_x2 <- 0
while(i<=N){
    Pr_almeno_x2 <- Pr_almeno_x2 + choose(N,i)*(p^i)*(q^(N-i))
    i <- i+1
}
Pr_almeno_x2

'
#Dado non bilanciato:
#	 1	|   2  |   3  |   4  |   5  |   6  |
#  0.22 | 0.18 | 0.26 | 0.15 | 0.12 | 0.07 |

#Probabilità delle varie faccie:
x1 <- 0.22;
x2 <- 0.18;
x3 <- 0.26;
x4 <- 0.15;
x5 <- 0.12;
x6 <- 0.07;

#Numero lanci
lanci = 18;

#Se esce 1 o 4 è un successo, tutti gli altri casi sono insuccessi.
#Evento elementare è un lancio del dado.

#P(X = x) = |N| * p^x * (1 - p)^{N-x}
#			|x|

F <- function(x){
	choose(lanci,x) * Pr_Successo_Evento_Elementare^x * (1 - Pr_Successo_Evento_Elementare)^(lanci - x);
}

#1: Qual è la probabilità di successo p dell evento elementare?7
	Pr_Successo_Evento_Elementare <- x1 + x4;
	Pr_Successo_Evento_Elementare

#2: Qual è la probabilità di ottenere ESATTAMENTE un successo (in N = 18 lanci)?
	Pr_1_successo <- choose(lanci,1) * Pr_Successo_Evento_Elementare^1 * (1-Pr_Successo_Evento_Elementare)^(lanci - 1);
	Pr_1_successo;
	Pr_1_successo2 <- F(1);
	Pr_1_successo2;

#3: Qual è la probabilità di ottenere ALMENO 5 successi (in N = 18 lanci)?
	Pr_0_successi <- F(0);
	Pr_2_successi <- F(2);
	Pr_3_successi <- F(3);
	Pr_4_successi <- F(4);
	
	Pr_almeno_5_successi <- 1 - (Pr_0_successi + Pr_1_successo + Pr_2_successi + Pr_3_successi + Pr_4_successi);
	Pr_almeno_5_successi
'