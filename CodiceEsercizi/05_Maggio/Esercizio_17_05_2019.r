library(MASS);
#Dati
lamda_x <- 0.6
lamda_y <- 0.6

Fu <- function(u,x){
	ifelse(x > u, exp(-lamda_y * u), exp((-(lamda_x + lamda_y)*u)) / (exp(-lamda_x * x)))
}

#1: Determinare la funzione di ripartizione di U := min(X,Y)
	
	function(u){return(ifelse(u<0, 0, pexp(u, lamda_x+lamda_y)))};
	function(u){return(ifelse(u<0, 0, 1 - exp(-(lamda_x+lamda_y) * u)))};
	function(u){return(pexp(u, lamda_x + lamda_y))};
	
#CONDIZIONATA:
# 			| 
#	 		| e^(-lamda_y * u) se x <= u
#Fu(u) =	|
# 			| e^(-(lamda_x + lamda_y) * u) / e^(-lamda_x * u)  se x > u
# 			|
	
#2: Determinare P(U > c | X > 0.666)
	pr_1 <- Fu(2.765, 0.666)
	cat("P(U > 0.049 | X > 0.297) ", pr_1, "\n")
	
#3: Determinare P(U > 0.666 | X > 2.765)
	pr_2 <- Fu(0.666, 2.765)
	cat("P(U > 0.297 | X > 0.049) ", pr_2, "\n")

#RISPOSTE:
#1: function(u){return(ifelse(u<0, 0, 1 - exp(-(0.6+0.6) * u)))};
#2: 0.05402005
#3: 0.6705882
