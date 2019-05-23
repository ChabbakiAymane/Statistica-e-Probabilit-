library(MASS)
#Sia data la seguente variabile aleatoria bivariata discreta
#   |  X\Y  | -2.15  | -0.96 | -0.8 |
#   | -0.34 |   2k   |   5k  |  1k  |
#   | 1.62  |   6k   |   8k  |  3k  |
#   | 2.65  |   7k   |   9k  |  4k  |


#Calcolo di k in modo che la tabella sia una variabile congiunta
#I le cose da cambiare qui sono i valori all'interno del vettore perMat e i nomi di righe e colonne
	perMat <- c(2,5,1,6,8,3,7,9,4)
	matrice <- matrix(perMat, nrow=3, ncol=3, byrow=TRUE)
	colnames(matrice) <- c(-2.15, -0.96, -0.8)
	rownames(matrice) <- c(-0.34, 1.62, 2.65)
	k <- 1/sum(matrice)
	matrice <- matrice*k
	marginaleX <- rowSums(matrice[, c(1, 2, 3)])
	marginaleY <- colSums(matrice[, c(1, 2, 3)])
	k
	fractions(k)

#Calcolo probabilità della marginaleX in x = -0.34
	#Mettere tra doppi apici il valore della x
	x <- "-0.34"
	Ris2 <- as.numeric(marginaleX[x])
	Ris2
	fractions(Ris2)

#Calcolo probabilità della marginaleY in y = -2.15
	#Mettere tra doppi apici il valore della x
	y <- "-2.15"
	Ris3 <- as.numeric(marginaleY[y])
	Ris3
	fractions(Ris3)

#Calcolo E(X)
	i <- 1
	eX <- 0
	while(i<=3){
	  eX <- eX + (as.numeric(names(marginaleX)[i])*as.numeric(marginaleX[i]))
	  i <- i + 1
	}
	eX

#Calcolo Var(Y) = e(Y^2) - (e(Y))2
	i <- 1
	eY <- 0
	while(i<=3){
	  eY <- eY + (as.numeric(names(marginaleY)[i])*as.numeric(marginaleY[i]))
	  i <- i + 1
	}
	eY

	i <- 1
	eY_ordine2 <- 0
	while(i<=3){
	  eY_ordine2 <- eY_ordine2 + (((as.numeric(names(marginaleY)[i]))^2)*as.numeric(marginaleY[i]))
	  i <- i + 1
	}
	eY_ordine2

	VarY <- eY_ordine2 - (eY)^2
	VarY

######################################################################################################################################
#Dati:

row0 <- c(-2.15,-0.96,-0.8)
col0 <- c(-0.34,1.62,2.65)

#Tabella
row1 <- c(2,5,1);
row2 <- c(6,8,3);
row3 <- c(7,9,4);

#1: Calcolare la constate k che deve essere utilizzata per rendere la tabella una funzione di probabilità congiunta
	k <- 1/sum(row1[1:3] + row2[1:3] + row3[1:3]);
	k
	fractions(k)
	
#2: Calcolare la distribuzione di probabilità marginale di X. Inserire il solo valore di probabilità per X = -0.34
	Pr_X <- sum(row1[1:3])*k;
	Pr_X
	fractions(Pr_X)
	
#3: Calcolare la distribuzione di probabilità marginale di Y. Inserire il solo valore di probabilità per Y = -2.15
	Pr_Y <- (row1[1] + row2[1] + row3[1])*k;
	Pr_Y
	fractions(Pr_Y)
	
#4: Calcolare il valore atteso di X.
	val_atteso_X_1 <- col0[1] * sum(row1)*k
	val_atteso_X_2 <- col0[2] * sum(row2)*k
	val_atteso_X_3 <- col0[3] * sum(row3)*k
	val_atteso <- val_atteso_X_1 + val_atteso_X_2 + val_atteso_X_3
	
	val_atteso
	fractions(val_atteso)
	
#5: Calcolare la varianza di Y.
#Var(Y) = E(Y^2) - E(Y)^2
	#E(Y)
	tmpX_1 <- row0[1] * sum(row1[1],row2[1],row3[1])*k
	tmpX_2 <- row0[2] * sum(row1[2],row2[2],row3[2])*k
	tmpX_3 <- row0[3] * sum(row1[3],row2[3],row3[3])*k
	val_atteso <- tmpX_1 + tmpX_2 + tmpX_3
	
	#E(Y^2)
	tmpY_1 <- (row0[1])^2 * sum(row1[1],row2[1],row3[1])*k
	tmpY_2 <- (row0[2])^2 * sum(row1[2],row2[2],row3[2])*k
	tmpY_3 <- (row0[3])^2 * sum(row1[3],row2[3],row3[3])*k
	momento_non_cent_2 <- tmpY_1 + tmpY_2 + tmpY_3
	
	varianza <- momento_non_cent_2 - (val_atteso)^2
	varianza
	fractions(varianza)
	
#RISULTATI:
#1: 1/45 == 0.02222222
#2: 8/45 == 0.1777778
#3: 1/3 == 0.3333333
#4: 1297/750 == 1.729333
#5: 79841/234140 == 0.3409968