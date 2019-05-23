library(MASS);

#f(x) = 2 * landa * x * exp{-landa * x^2} 	x,landa > 0
#f(y) = alpha * exp{-alpha * x}	x >= 0

#landa
landa = 1.05

#alpha
alpha <- 0.69

#funzione F
fx <- function(x){
	return(2 * landa * x * exp(-landa*x^2));
}

#funzione Y
fy <- function(y){
	return(alpha * exp((-alpha)*y));
}

#funzione momento 1
fm1 <- function(x){
	return(x * fy(x));
}

#funzione momento 2
fm2 <- function(x){
	return(x^2 * fy(x));
}

#1: Calcolare P(X <= 0.97)
	inf_1 <- 0
	max_1 <- 0.97

	Pr_1 <- integrate(fx, inf_1, max_1, stop.on.error = FALSE)$value;
	Pr_1;

#2: Calcolare P(0.66 <= X <= 1.18)
	inf_2 <- 0.66
	max_2 <- 1.18

	Pr_2 <- integrate(fx, inf_2, max_2, stop.on.error = FALSE)$value;
	Pr_2;

#3: Momenti
#3.1: Calcolare momento (non centrato) di ordine 1
	Pr_3_1 <- integrate(fm1, 0, Inf, stop.on.error = FALSE)$value;
	Pr_3_1;

#3.2: Calcolare momento (non centrato) di ordine 2
	Pr_3_2 <- integrate(fm2, 0, Inf, stop.on.error = FALSE)$value;
	Pr_3_2;


#1: 0.6276589
#2: 0.4011721
#3.1: 1.449275
#3.2: 4.200798
