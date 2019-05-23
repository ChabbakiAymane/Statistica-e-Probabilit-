library(MASS);
#Sia dato lo spazio probabilizzato (Ω,A,Pr) dove: Ω=R, A=B(R) e Pr e definita nei seguenti singoletti di R
#	|  x1  |  x2  |  x3  |  x4  |  x5  |  x6  |  x7  |  x8  |
#	| 1.00 | 2.00 | 3.00 | 4.00 | 5.00 | 6.00 | 7.00 | 8.00 | 
#p	| 0.08 | 0.24 |	0.00 | 0.23 | 0.14 | 0.13 | 0.15 | 0.03 |

#Definiamo la probabilità dei singoletti
	x1 <- 0.08
	x2 <- 0.24
	x3 <- 0.00
	x4 <- 0.23
	x5 <- 0.14
	x6 <- 0.13
	x7 <- 0.15
	x8 <- 0.03

	v <- c(x1, x2, x3, x4, x5, x6, x7, x8)
	v
	
#1: Qual è la probabilità dell'intervallo (3,7)?
#Probabilità intervallo (a,b)
	a <- 3
	b <- 7
	Pr_ab_esclusi <- 0
	i <- a+1
	while(i < b){
	   Pr_ab_esclusi <- Pr_ab_esclusi + v[i]
	   i <- i+1
	}
	Pr_ab_esclusi
	
#2: Qual è la probabilità dell'intervallo (6,+∞)?
#Probabilità intervallo (6,+infinity)
	a <- 6
	Pr_a_infinity_esclusi <- 0
	i <- a+1
	while(i <= 8){
	   Pr_a_infinity_esclusi <- Pr_a_infinity_esclusi + v[i]
	   i <- i+1
	}
	Pr_a_infinity_esclusi
	
#3: Qual è il valore della funzione di distribuzione nel punto x=6?
#Probabilità della funzione di distribuzione nel punto y
	y <- 6
	i <- 1
	Pr_y <- 0
	while(i < y+1){
	   Pr_y <- Pr_y + v[i]
	   i <- i+1
	}
	Pr_y

#4: Il valore della funzione di distribuzione in z_1=1 è maggiore di quello in z_2=7?
	z1 <- 1
	z2 <- 7
	Pr_z1 <- 0
	Pr_z2 <- 0
	
	i <- 1
	while(i < z1+1){
	   Pr_z1 <- Pr_z1 + v[i]
	   i <- i+1
	}
	Pr_z1
	
	i <- 1
	while(i < z2+1){
	   Pr_z2 <- Pr_z2 + v[i]
	   i <- i+1
	}
	Pr_z2
	
	#z_1 > z_2?
	Pr_z1 > Pr_z2
