library(MASS);
#DATI
x_values <- c(-3,5,6)
y_values <- c(-2,0,3,7)
prxs <- c(0.38,0.29,0.33)
prys <- c()
prCongiunta <- c()

#MARGINALE X
pry_x <- c(0.19, 0.25, 0.31, 0.25,
           0.28, 0.29, 0.23, 0.20,
           0.21, 0.26, 0.21, 0.32)

#MATRICE
matrixy_x <- matrix(data = pry_x, ncol = length(y_values), nrow = length(x_values), byrow = TRUE);

#1: Calcolare E(X)
	#VALORE ATTESO X
	valoreAttesoX <- 0
	for( i in 1:length(x_values) ){
	  valoreAttesoX <- valoreAttesoX + prxs[i] * x_values[i];
	}

#2: Calcolare Var(X)
	#VARIANZA X
	varianzaX <- 0
	for( i in 1:length(x_values) ){
	  varianzaX <- varianzaX + prxs[i] * (x_values[i] - valoreAttesoX)^2;
	}

#VALORI CONGIUNTA
for( i in 1:length(x_values) ){
  for( j in 1:length(y_values) ){
    prCongiunta <- c(prCongiunta, matrixy_x[i,j] * prxs[i]);
  }
}

#MATRICE CONGIUNTA
matrixCongiunta <- matrix(data = prCongiunta, nrow = length(x_values), ncol = length(y_values), byrow = TRUE);
for( i in 1:length(y_values) ){
  prys <- c(prys, sum(matrixCongiunta[,i]));
}

#MARGINALE Y
prx_y <- c()
for( i in 1:length(x_values)){

  for( j in 1:length(y_values)){
    prx_y <- c(prx_y, matrixCongiunta[i,j] / prys[j] )
  }

}

matrixx_y <- matrix(data=prx_y, nrow = length(x_values), ncol = length(y_values), byrow = TRUE);

#3: Calcolare Var(E(X|Y))
	#E(X|Y)
	valoreAttesoX_Y <- c();

	for( j in 1:length(y_values) ){
	  somma <- 0
	  for( i in 1:length(x_values) ){
		somma <- somma + x_values[i] * matrixx_y[i,j];
	  }
	  valoreAttesoX_Y <- c(valoreAttesoX_Y, somma);
	}

	#Var(E(X|Y))
	varianzaValoreAttesoX_Y <- 0
	for( j in 1:length(y_values) ){
	  varianzaValoreAttesoX_Y <- varianzaValoreAttesoX_Y + (valoreAttesoX_Y[j] - valoreAttesoX)^2 * prys[j];
	}

#4: Calcolare E(Var(X|Y))
	#E(Var(X|Y)) =  Var(X) - Var(E(X|Y))
	valoreAttesoVarianzaX_Y <- varianzaX - varianzaValoreAttesoX_Y;

#RISPOSTE:
	cat("X\n")
	print(matrix(data=c(x_values,prxs), ncol = length(x_values), byrow = TRUE))

	cat("\nY\n")
	print(matrix(data=c(y_values,prys), ncol = length(y_values), byrow = TRUE))

	cat("\nProbabilita congiunta\n")
	print(matrixCongiunta)

	cat("\nX|Y\n")
	print(matrixx_y)

	cat("\nY|x\n")
	print(matrixy_x)

	cat("\nRisposte\n")
	print(valoreAttesoX);
	print(varianzaX)
	print(varianzaValoreAttesoX_Y)
	print(valoreAttesoVarianzaX_Y)
	
#RISULTATI:
	#1: 2.29
	#2: 17.3059
	#3: 0.1915314
	#4: 17.11437