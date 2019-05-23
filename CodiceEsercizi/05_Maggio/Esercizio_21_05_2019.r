library(MASS)
#Siano X e Y due variabili aleatorie distribuite secondo una distribuzione di Poisson di parametro lambda1 e lambda2
lamdaX <- 10
lambdaY <- 4

#1: Determinare E(X + Y) e poi, usando la disuguaglianza di Markov il limite superiore della probabilità condizionata con somma maggiore o uguale a n
	# Sia Z = X + Y
	# E(Z) = E(X) + E(Y)
	n <-21
	valoreAttesoZ <- lamdaX + lambdaY
	limSup <- valoreAttesoZ/n
	cat("E(X+Y) =",fractions(limSup), "\n")

#2: Usando la disuguaglianza di Markov ottenere P(X >= x | X + Y = n)
	x <- 6
	# Si ha che px|Z(X|Z = n) è una binomiale di parametro n = n e p = lamdaX/(lamdaX + lamdaY)
	# Il valore atteso della binomiale è: E(Binom(n, p)) = np
	# Allora:
	limSupCond <- (n * lamdaX/(lamdaX + lambdaY))/x
	cat("Markov:", limSupCond, "\n")

#3: Sia X una normale standard e Y una variabile aleatoria definita da: fattoreMoltiplicativo * X, 
#   usando Chebychev per ottenere un limite inferiore della probabilità P(|Y| < y)
	y <- 2.6
	fattoreMoltiplicativo <- 1.1
	limInf <- 1 - ((fattoreMoltiplicativo)^2)/(y^2)
	cat("Chebychev:", limInf, "\n")

#4: Quale è il valore esatto di P(|Y| < 2.6)
	prob <- integrate(dnorm, -y/(fattoreMoltiplicativo), y/(fattoreMoltiplicativo))$value
	cat("P(|Y| < 2.6) =", prob, "\n")
	
#RISPOSTE:
	#1: 2/3 = 0.6666667
	#2: 2.5
	#3: 0.8210059
	#4: 0.9819034