library(MASS)
#Sia data la seguente variabile aleatoria bivariata discreta
#   |  X\Y  | -3.35  | -0.51 | 1.32 |
#   | -2.68 |   9k   |   12k |  3k  |
#   | 3.56  |   2k   |   10k |  8k  |
#   | 3.88  |   11k  |   6k  |  1k  |


#Calcolo di k in modo che la tabella sia una variabile congiunta
#I le cose da cambiare qui sono i valori all'interno del vettore perMat e i nomi di righe e colonne
perMat <- c(9,12,5,3,2,10,7,8,11,6,4,1)
matrice <- matrix(perMat, nrow=3, ncol=4, byrow=TRUE)
colnames(matrice) <- c(-3.35, -0.51, 0.37, 1.32)
rownames(matrice) <- c(-2.68, 3.56, 3.88)
k <- 1/sum(matrice)
matrice <- matrice*k
marginaleX <- rowSums(matrice[, c(1, 2, 3, 4)])
marginaleY <- colSums(matrice[, c(1, 2, 3, 4)])
k
fractions(k)

#   | X \ Y | 	-3.35  |  -0.51  |  1.32  |
#   | -2.68 |   9/78   |  12/78  |  3/78  |
#   | 3.56  |   2/78   |  10/78  |  8/78  |
#   | 3.88  |   11/78  |  6/78   |  1/78  |

#Calcolo probabilità della marginaleX P(X<=-2.68)
	x <- -2.68
	i <- 1
	Ris2 <- 0
	repeat{
		Ris2 <- Ris2 + marginaleX[i]
		
		if(((as.numeric(names(marginaleX)[i]))==x)){
			break
		}
		i <- i+1
	}
	Ris2 <- as.numeric(Ris2)
	fractions(Ris2)

#Calcolo probabilità della marginaleY P(X>-3.35)
	y <- -3.35
	i <- 1
	Ris3 <- 0
	repeat{
		Ris3 <- Ris3 + marginaleY[i]
		
		if(((as.numeric(names(marginaleY)[i]))==y)){
			break
		}
		i <- i+1
	}
	Ris3 <- as.numeric(Ris3)
	Ris3 <- 1 - Ris3
	fractions(Ris3)

#Calcolo E(X|Y=-0.51)
	y2 <- -0.51
	matriceX_dato_Y <- matrice
	i <- 1
	while(i<=4){
		if(as.numeric(colnames(matrice)[i])==y2){
			matriceX_dato_Y <- matrice[, i]/marginaleY[i]
		}
		i<-i+1
	}

	i <- 1
	eX_dato_Y <- 0
	while(i<=3){
	  eX_dato_Y <- eX_dato_Y + (as.numeric(names(matriceX_dato_Y)[i])*as.numeric(matriceX_dato_Y[i]))
	  i <- i + 1
	}
	eX_dato_Y

#Calcolo Var(Y|X=-2.68)
	x2 <- -2.68
	matriceY_dato_X <- matrice
	i <- 1
	while(i<=3){
		if(as.numeric(rownames(matrice)[i])==x2){
			matriceY_dato_X <- matrice[i ,]/marginaleX[i]
		}
		i<-i+1
	}

	i <- 1
	eY_dato_X <- 0
	while(i<=4){
	  eY_dato_X <- eY_dato_X + (as.numeric(names(matriceY_dato_X)[i])*as.numeric(matriceY_dato_X[i]))
	  i <- i + 1
	}
	#E(Y|X=-2.68) 
	eY_dato_X

	i <- 1
	eY_dato_X_2 <- 0
	while(i<=4){
	  eY_dato_X_2 <- eY_dato_X_2 + ((as.numeric(names(matriceY_dato_X)[i])^2)*as.numeric(matriceY_dato_X[i]))
	  i <- i + 1
	}
	#E(Y^2|X=-2.68) 
	eY_dato_X_2

	#Var(Y^2|X=-2.68) = E(Y^2|X=-2.68) - E[(Y^2|X=-2.68)^2]
	VarY_dato_X <- eY_dato_X_2 - eY_dato_X^2
	VarY_dato_X
	
	
#RISULTATI:
#1: 1/78 == 0.01282051
#2: 29/78 == 0.3717949
#3: 28/39 == 0.7179487
#4: 0.9542857
#5: 2.6911 == 3.794324 - (-1.050345)^2