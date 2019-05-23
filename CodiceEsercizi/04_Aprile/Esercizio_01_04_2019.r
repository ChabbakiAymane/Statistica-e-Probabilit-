library(MASS);

#Funzione definita su tutto R:
#	        |     1          |^2
#	 F(x) = |----------------|
#	        |1 + e^(-0.7 * x)|

#Costante della funzione
costante <- -0.7;

F <- function(x){
	tmp <- 1/(1 + exp(costante * x))^2;
	return (tmp);
}

#1: Qual'è la probabilità di: (-inf, -0.42]?
	#Pr(-inf, -0.42] = Pr(-0.42) - Pr(-inf)
	#Pr(-inf) = 0
	Pr_intervallo1 = F(-0.42) - F(-Inf);
	Pr_intervallo1;

#2: Qual'è la probabilità di: (1.17, +inf)?
	#Pr(1.17, +inf) = Pr(+inf) - Pr(1.17)
	#Pr(+inf) = 1
	Pr_intervallo2 = F(Inf) - F(1.17);
	Pr_intervallo2;

#3: Qual'è la probabilità di: (-inf, -0.42] ^ (1.17, +inf) U 0?
	#Pr((-inf, -0.42] ^ (1.17, +inf) U 0)) = Pr(Insieme vuoto U 0) = Pr(0)
	Pr_intervallo3 = 0;
	Pr_intervallo3;

#4: Qual'è la probabilità di: R \ ((-inf, -0.42] ^ (1.17, +inf) U (0, 0.17))?
	#Pr((-inf, -0.42] ^ (1.17, +inf) U (0, 0.17)) = Pr(Insieme vuoto U (0, 0.17)) = Pr(0, 0.17)
	#Pr(R \ ((-inf, -0.42] ^ (1.17, +inf) U (0, 0.17))) = Pr(R) - Pr((-inf, -0.42] ^ (1.17, +inf) U (0, 0.17))
	#Quindi si ha Pr(R \ ((-inf, -0.42] ^ (1.17, +inf) U (0, 0.17))) = Pr(R) - Pr((0, 0.17))
	
	Pr_tmp_intervallo4 = F(0.17) - F(0);
	Pr_intervallo4 = 1 - Pr_tmp_intervallo4;
	Pr_intervallo4;