library(MASS)

#Le cose da cambiare qui sono i valori all'interno del vettore perMat e i nomi di righe e colonne
perMat <- c(0.0044, 0.0104,0.0048,0.0104,0.0032,0.0068,0.0077,0.0182,0.0084,0.0182,0.0056,0.0119,0.0418,0.0988,0.0456,0.0988,0.0304,0.0646,0.0022,0.0052,0.0024,0.0052,0.0016,0.0034,0.0231,0.0546,0.0252,0.0546,0.0168,0.0357,0.0308,0.0728,0.0336,0.0728,0.0224,0.0476)
matrice <- matrix(perMat, nrow=6, ncol=6, byrow=TRUE)
colnames(matrice) <- c(1, 2, 3, 4, 5, 6)
rownames(matrice) <- c(1, 2, 3, 4, 5, 6)
sum(matrice)
marginaleX <- rowSums(matrice[, c(1, 2, 3, 4, 5, 6)])
marginaleY <- colSums(matrice[, c(1, 2, 3, 4, 5, 6)])

#1. esce un numero pari sul primo dado
	i <- 2
	Pr_pari <- 0
	while(i <= 6){
		Pr_pari <- Pr_pari + as.numeric(marginaleX[i])
		i <- i + 2
	}
	Pr_pari <- Pr_pari
	Pr_pari

#2. non escono 4 o 5 sul secondo dado
	x1 <- 4
	x2 <- 5

	Pr_estratti <- sum(marginaleY) - (marginaleY[x1] + marginaleY[x2])
	Pr_estratti <- as.numeric(Pr_estratti)
	Pr_estratti

#3.PX|Y (X|Y = 5) e determinare Pr(X>=4)
	y <- 5
	x <- 4
	matriceX_dato_Y <- matrice
	i <- 1
	while(i<=6){
		if(as.numeric(colnames(matrice)[i])==y){
			matriceX_dato_Y <- matrice[, i]/marginaleY[i]
		}
		i<-i+1
	}

Pr_maggiore_x_dato_y <- 0
i <- x
while(i<=6){
    Pr_maggiore_x_dato_y <- Pr_maggiore_x_dato_y + as.numeric(matriceX_dato_Y[i])
    i<-i+1
}
Pr_maggiore_x_dato_y

#4. E(X|Y = 4)
	y1 <- 4
	matriceX_dato_Y2 <- matrice
	i <- 1
	while(i<=6){
		if(as.numeric(colnames(matrice)[i])==y1){
			matriceX_dato_Y2 <- matrice[, i]/marginaleY[i]
		}
		i<-i+1
	}

	i <- 1
	eX_dato_Y <- 0
	while(i<=6){
	  eX_dato_Y <- eX_dato_Y + (as.numeric(names(matriceX_dato_Y2)[i])*as.numeric(matriceX_dato_Y2[i]))
	  i <- i + 1
	}
	eX_dato_Y

#5. Var[Y|X = 4]
	x3 <- 4
	matriceY_dato_X <- matrice
	i <- 1
	while(i<=6){
		if(as.numeric(rownames(matrice)[i])==x3){
			matriceY_dato_X <- matrice[i ,]/marginaleX[i]
		}
		i<-i+1
	}

	i <- 1
	eY_dato_X <- 0
	while(i<=6){
	  eY_dato_X <- eY_dato_X + (as.numeric(names(matriceY_dato_X)[i])*as.numeric(matriceY_dato_X[i]))
	  i <- i + 1
	}
	eY_dato_X

	i <- 1
	eY_dato_X_2 <- 0
	while(i<=6){
	  eY_dato_X_2 <- eY_dato_X_2 + ((as.numeric(names(matriceY_dato_X)[i])^2)*as.numeric(matriceY_dato_X[i]))
	  i <- i + 1
	}
	eY_dato_X_2

	VarY_dato_X <- eY_dato_X_2 - eY_dato_X^2
	VarY_dato_X
	
#SOLUZIONI:
#1: 0.37 == (0.07+0.02+0.28)
#2: 0.66 == 1-(0.26+0.08)
#3: 0.51
#4: 4.13
#5: 2.6075 == 14.51 - (3.45)^2