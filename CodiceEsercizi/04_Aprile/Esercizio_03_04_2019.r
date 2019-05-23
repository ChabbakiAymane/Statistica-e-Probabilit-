library(MASS);

#FUNZIONE DI RIPARTIZIONE
#		---
#		| 0			x < −1.916
#		| 0.0004	−1.916 ≤ x < −0.963
#		| 0.0063 	−0.963 ≤ x < −0.01
#		| 0.0469	−0.01 ≤ x < 0.943
#F(x)= -|
#		| 0.1961	0.943 ≤ x < 1.896
#		| 0.5044 	1.896 ≤ x < 2.849
#		| 0.8441 	2.849 ≤ x < 3.802
#		| 1			3.802 ≤ x
#		---

x_0 <- 0;
x_1 <- 0.0004;
x_2 <- 0.0063;
x_3 <- 0.0469;
x_4 <- 0.1961;
x_5 <- 0.5044;
x_6 <- 0.8441;
x_7 <- 1;

#Funzione di distribuzione
v <- c(x_0, x_1, x_2, x_3, x_4, x_5, x_6, x_7);
v

#Funzione di probabilità P
p <- c(x_1 - x_0, x_2 - x_1, x_3 - x_2, x_4 - x_3, x_5 - x_4, x_6 - x_5, x_7 - x_6);
p

F <- function(x){
	if(x < -1.916){ return (x_0); }
	if(x >= −1.916 && x < −0.963){ return (x_1); }
	if(x >= −0.963 && x < −0.01){ return (x_2); }
	if(x >= −0.01 && x < 0.943){ return (x_3); }
	if(x >= 0.943 && x < 1.896){ return (x_4); }
	if(x >= 1.896 && x < 2.849){ return (x_5); }
	if(x >= 2.849 && x < 3.802){ return (x_6); }
	if(x >= 3.802){ return (x_7); }
}

P <- function(x){
	if(x ==  -1.916) { return (x_1 - x_0); }
	if(x ==  −0.963) { return (x_2 - x_1); }
	if(x ==  −0.01) { return (x_3 - x_2); }
	if(x ==  0.943) { return (x_4 - x_3); }
	if(x ==  1.896) { return (x_5 - x_4); }
	if(x ==  2.849) { return (x_6 - x_5); }
	if(x ==  3.802) { return (x_7 - x_6); }
}

#1: Qual è la probabilità dell'intervallo (-0.963, 2.47]?
	Pr_F_intervallo_1 <- F(2.47) - F(-0.963);
	Pr_F_intervallo_1

	Pr_P_intervallo_1 <- P(−0.01) + P(1.896) + P(0.943);
	Pr_P_intervallo_1;
	
#2: Qual è la probabilità dell'intervallo [0.943, 1.896)?
	Pr_F_intervallo_2 <- F(1.896) - F(0.943) - P(1.896) + P(0.943);
	Pr_F_intervallo_2
	
	Pr_P_intervallo_2 <- P(0.943);
	Pr_P_intervallo_2;

#3: Dalla funzione di probabilità P(x), determinare qual è il valore minimo di x per cui P(x)=0.3083
	x <- 1.896
	x
	P(x);
