library(MASS)
#X ~ Poiss(lamda)
#Pr(X = x) = (lamda^x / x!) * exp(-lamda) x >= 0
#			 0 altrimenti

#Dati
lamda = 5;

#1: Calcolare la probabilità che più di 9 eventi si verifichino in una settimana.
	eventi <- 9;
	Pr_1 <- 1 - sum(dpois(0:eventi, lamda));
	Pr_1
	
#2: Calcolare la probabilità che al più 13 eventi si verifichino in una settimana.
	eventi <- 13;
	Pr_2 <- sum(dpois(0:eventi, lamda));
	Pr_2
	
#3: Calcolare la probabilità che trascorrano almeno 6 settimane tra due eventi successivi.
	#Funzione esponenziale
	F <- function(x){return(lamda * exp(-lamda * x))}
	#Dati
	giorni <- 6;
	
	#Pr(X>=6) = 1 - Pr(X < 6) = 1 - (1 - exp{-lamda * x}) = exp{-lamda * x}
	Pr_3 <- exp(-lamda * giorni);
	Pr_3
	
	#Usando l'integrale da 6 a +Inf
	Pr_prova1 <- integrate(F,giorni,Inf)$value;
	Pr_prova1
	#1 - integrale da -Inf a 6
	Pr_prova2 <- 1 - integrate(F,0,giorni)$value;
	Pr_prova2
	#Usando formula di R per esponenziale
	Pr_prova3 <- 1 - pexp(giorni,lamda);
	Pr_prova3
	
#RISULTATI:
#1: 0.03182805731 == 1 - sum(dpois(0:9, 5))
#2: 0.99930201 == sum(dpois(0:13, 5))
#3: 9.357622969e-14 == exp(-5 * 6) 
#3: 9.35918e-14 == 1 - pexp(6,5)