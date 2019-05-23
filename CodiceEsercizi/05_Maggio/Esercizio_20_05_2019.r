library(MASS)
#R~(sigma_1, mu_1)
#G~(sigma_2, mu_2)

#Dati
mu_1 <- 34
mu_2 <- 26

sigma_1 <- 9
sigma_2 <- 6

prezzo <- 0.06

#1: Quanto costa in media un sacchetto di caramelle?
	costoMedio <- (mu_1 + mu_2)*prezzo
	cat("Costo medio di un sacchetto =", costoMedio, "\n")

#2: Quanto cosa in media un sacchetto di caramelle, con i nuovi prezzi?
	prezzo_rosse <- 0.19
	prezzo_gialle <- 0.06
	
	costoMedio <- (mu_1 * prezzo_rosse) + (mu_2 * prezzo_gialle)
	cat("Costo medio di un sacchetto =", costoMedio, "\n")

#3: Qual'è la varianza del prezzo di un sacchetto di caramelle (con i nuovi prezzi)?
	#Var(X) = Var(prezzo_rosse * R) + Var(prezzo_gialle * R) + 0 (covariaza vale 0)
	varianza <- (prezzo_rosse^2 * sigma_1^2)+(prezzo_gialle^2 * sigma_2^2)
	cat("Varianza del prezzo di un sacchetto =", varianza, "\n")
	
#4: Disugualianza di Markov.
	alpha <- 7.13
	limite <- costoMedio / alpha
	cat("Disugualianza di Markov =", limite, "\n")
	
#RISPOSTE:
#1: 3.6
#2: 8.02
#3: 3.0537
#4: 1.124825