library(MASS);
#DATI
x_values <- c(-7.5, 0, 6.5)
y_values <- c(-9.5, -1.5, 6.5, 8.5)
prxs <- c(0.30,0.39,0.31)
prys <- c()
prCongiunta <- c()

pry_x <- c(0.29, 0.24, 0.24, 0.23,
           0.18, 0.25, 0.30, 0.27,
           0.28, 0.30, 0.24, 0.18)
		   
#Matrice X
matrixy_x <- matrix(data = pry_x, ncol = length(y_values), nrow = length(x_values), byrow = TRUE);

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

#1: Qual'è la probabilità di (-7.5, 6.5)
	#Pr(-7.5, 6.5)
	indiceX <- 1
	indiceY <- 3
	pr <- matrixCongiunta[indiceX,indiceY]
	
#2: Determinare Cov(X,Y)
	#VALORE ATTESO Y
	valoreAttesoY <- 0
	for( i in 1:length(y_values) ){
	  valoreAttesoY <- valoreAttesoY + prys[i] * y_values[i];
	}

	#VARIANZA Y
	varianzaY <- 0
	for( i in 1:length(y_values) ){
	  varianzaY <- varianzaY + prys[i] * (y_values[i] - valoreAttesoY)^2;
	}

	#VARIANZA X*Y
	varianzaX_Y <- 0
	for( i in 1:length(x_values) ){
	  for( j in 1:length(y_values) ){
		varianzaX_Y <- varianzaX_Y + (x_values[i] * y_values[j] * matrixCongiunta[i,j]);
	  }
	}
	
	#COVARIANZA
	covarianza <- varianzaX_Y - (valoreAttesoX*valoreAttesoY);

#3: X e Y sono non correlate?
	(covarianza == 0)

#4: Determinare p(X,Y)
	#INDICE DI CORRELAZIONE
	indice_correlazioe <- covarianza / sqrt(varianzaX * varianzaY)

#RISPOSTE
	cat("X\n")
	print(matrix(data=c(x_values,prxs), ncol = length(x_values), byrow = TRUE))

	cat("\nY\n")
	print(matrix(data=c(y_values,prys), ncol = length(y_values), byrow = TRUE))

	cat("\nProbabilita congiunta\n")
	print(matrixCongiunta)

	cat("\nY|X\n")
	print(matrixy_x)

	cat("\nRisposte\n")
	print(pr)
	print(valoreAttesoX);
	print(varianzaX);
	print(valoreAttesoY);
	print(varianzaY);
	print(covarianza);
	print(indice_correlazioe);

#RISULTATI
	#1: 0.072
	#2: -0.715593
	#3: FALSE
	#4: -0.01860454