library(MASS);
#DATI
x_values <- c(-10, -8.5, -5.5, -1.5)
y_values <- c(1, 2, 3)

prxs <- c()
prys <- c(0.28, 0.20, 0.52)
prCongiunta <- c()

prx_y <- c(0.26, 0.28, 0.21, 0.25,
           0.22, 0.18, 0.31, 0.29,
           0.18, 0.27, 0.31, 0.24)
		   
#Matrice X
matrixx_y <- matrix(data = prx_y, ncol = length(x_values), nrow = length(y_values), byrow = TRUE);

#VALORI CONGIUNTA
for( i in 1:length(y_values) ){
  for( j in 1:length(x_values) ){
    prCongiunta <- c(prCongiunta, matrixx_y[i,j] * prys[i]);
  }
}

#MATRICE CONGIUNTA
matrixCongiunta <- matrix(data = prCongiunta, nrow = length(y_values), ncol = length(x_values), byrow = TRUE);
for( i in 1:length(x_values) ){
  prxs <- c(prxs, sum(matrixCongiunta[,i]));
}

#VALORE ATTESO X
valoreAttesoX <- 0
for( i in 1:length(x_values) ){
  valoreAttesoX <- valoreAttesoX + prxs[i] * x_values[i];
}

#VARIANZA X
varianzaX <- 0
for( i in 1:length(x_values) ){
  varianzaX <- varianzaX + prxs[i] * (x_values[i] - valoreAttesoX)^2;
}

#RISPOSTE
	cat("X\n")
	print(matrix(data=c(x_values,prxs), ncol = length(x_values), byrow = TRUE))
	
	cat("\nY\n")
	print(matrix(data=c(y_values,prys), ncol = length(y_values), byrow = TRUE))

	cat("\nY|X\n")
	print(matrixx_y)
	
	cat("\nProbabilita congiunta\n")
	print(matrixCongiunta)
	
	cat("\nRisposte\n")
	print(valoreAttesoX);
	print(varianzaX);
	
#RISULTATI
	#1: Tempo medio per raggiungere la città: 11
	#2: Determinare E(X) = -6.2
	#3: Determinare Var(X) = 10.1086