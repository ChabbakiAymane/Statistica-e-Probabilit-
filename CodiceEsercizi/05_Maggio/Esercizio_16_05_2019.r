library(MASS);

#1: Determinare il valore di lamdax
	# P(X = 9) = P(X = 10)
	# (exp(-lamda_x) * lamda_x^9 ) / 9! = (exp(-lamda_x) * lamda_x^10) / 10!
	# 10 * lamda_x^9 = lamda_x^10
	# lamda_x^9(10 - lamda_x) = 0
	
	#X~Poisson(lamda_x)
	lamda_x <- 10
	cat("\nValore lamda_x: ", lamda_x, "\n");

#X indipendente da Y
#Y~Poisson(lamda_y)
lamda_y <- 4

#Z = X+Y ~ Poisson(lamda_x + lamda_y)
lamda_z <- lamda_x + lamda_y

Fx <- function(x){ return((exp(-x) * lamda_x^x) / factorial(x));}
Fy <- function(y){ return((exp(-y) * lamda_y^y) / factorial(y));}
Fz <- function(z){ return((exp(-z) * lamda_z^z) / factorial(z));}

#2: Qual'è la probabilità P(X = 12|Z = 15)
	pr_1 <- Fx(12) * Fy(3) / Fz(15)
	cat("\nProbabilità P(X = 12|Z = 15)= ", pr_1 ,"\n");
	
#3: Determinare il valore atteso condizionato E(X|Z = 15)
	#E(X|Z=n) = n * lamda_x / (lamda_x + lamda_y)
	n <- 15
	val_atteso <- n * lamda_x / (lamda_x + lamda_y)
	cat("\nE(X|Z = 15)= ", val_atteso ,"\n");

#RISPOSTE:
#1: 10
#2: 0.1871849
#3: 10.71429 == 15*10/14