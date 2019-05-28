library(MASS)
#Dati
campione <- c(84.50, 73.30, 87.30, 76.20, 85.50, 85.10, 87.70, 69.30, 81.10)

#STIMATORI
stimatore_mu <- function  (campione){
    lunghezza <- length(campione)
    somma <- 0
    for(i in 1:lunghezza){
        somma <- somma + campione[i]
    }
    #print(somma)
    #print(lunghezza)
    return (somma/lunghezza)
}

stimatore_sigma2 <- function(campione){
    lunghezza <- length(campione)
    mu <- stimatore_mu(campione)
    somma <- 0
    for(i in 1:lunghezza){
        somma <- somma + (campione[i]-mu)^2
    }
    #print(somma)
    #print(lunghezza)
    return (somma/lunghezza)
}

#1: Calcolare una stima puntuale del parametro toracico medio.
	mu <- stimatore_mu(campione);
	cat("Stima puntuale del parametro toracico medio:",mu,"\n") 
	fractions(mu)

#2: Calcolare una stima puntuale della deviazione standard, utilizzando lo stimatore.
	sigma2 <- stimatore_sigma2(campione);
	sigma2
	fractions(sigma2) 

	sigma <- sqrt(sigma2)
	cat("Stima puntuale della deviazione standard:",sigma,"\n")
	fractions(sigma) 

#3: Lo stimatore S^2(X1,…,X9) del punto precedente è non-distorto?
	fattore <- length(campione) / (length(campione)-1)
	cat("Costante moltiplicativa:",fattore,"\n")
	fractions(fattore)
	
#RISPOSTE:
#1: 81.11111
#2: 39.22321
#3: 6.262844