library(MASS);

#Dati
mu <- 5;
sigma <- 0.57;

#Soglie:
y <- 4.2695156;
x <- 5.7642304;

#Standardizzazione
	sx <- (x - mu)/sigma;
	sy <- (y - mu)/sigma;

#1: Qual è la probabilità che una caramella sia sopra il peso soglia?
	Pr_SopraSoglia <- 1 - pnorm(x, mu, sigma);
	Pr_SopraSoglia

#2: In media ogni quante caramelle se ne presenta una da scartare?
	p_scartare <- pnorm(y, mu, sigma) + pnorm(x, mu, sigma, lower.tail=FALSE)
	T <- 1/ p_scartare
	T

#3: Qual è il valore standardizzato corrispondente a 4.2695156?
	#1: valore standardizzato
	sy
	#2: valore standardizzato in modulo
	abs(sy)

#1: 0.08999999 == 1 - pnorm(5.7642304, 5, 0.57)
#2: 5.263158 
#3: -1.281552 == (4.2695156 - 5)/0.57