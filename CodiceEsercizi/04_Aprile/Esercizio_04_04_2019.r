if(!require(Ryacas)){
    install.packages("Ryacas",repos = "http://cran.us.r-project.org");
}

library(MASS);
library(Ryacas);

#FUNZIONE di RIPARTIZIONE e di DISTRIBUZIONE (mista)
#		---
#		| 0					x < 1
#		| 0.185				1 ≤ x < 2
#		| 0.375				2 ≤ x < 3
#F(x)= -| 0.5				x = 3
#		| 0.5 + 1/4(x−3)	3 < x ≤ 5
#		| 1					x > 5
#		---

F<- function(x){
	if(x < 1) {return (0)}
	if( x >= 1 && x < 2) {return (0.185)}
	if(x >= 2 && x < 3) {return (0.375)}
	if(x == 3) {return (0.5)}
	if(x > 3 && x <= 5) {
		tmp <- 0.5 + (0.25*(x−3));
		return (tmp)
	}
	if(x > 5) {return (1)}
}

#1: Qual è la probabilità dell'intervallo (−∞,2)?
	x <- Sym("x")
	val <- Limit(F(2 - x), x, -Inf)
	Pr_intervallo1 = F(val) - F(-Inf)
	Pr_intervallo1

#2: Qual è la probabilità dell'intervallo [4,∞)?
	

#3: Qual è la probabilità dell'intervallo [2,3)?
	

#4: Qual è la probabilità dell'intervallo [3,7)?
	
