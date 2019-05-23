library(MASS);

#epsilon
epsilon <- 1.056;

#Funzione: 
#F(x) = 1 - exp(-epsilon * x) se x>= 0 
#     = 0 altrimenti

F <- function(x) {
	if(x < 0){
		tmp <- 0;
	}else if(x > 1){
		tmp <- 1;
	}else{
		tmp <- (1 - exp(-epsilon * x));
	}
	return (tmp);
}

#F2 <- function (x, y){
#	if(x < 0){
#		if(y < 0){
#			tmp <- 0;
#		}else{
#			if(y > 1){
#				tmp <- -1;
#			}else{
#				tmp <- exp(-epsilon * y);
#			}
#		}
#	}else{
#		if(y < 0){
#			if(x > 1){
#				tmp <- -1;
#			}else{
#				tmp <- -exp(-epsilon * x);
#			}
#		}else{
#			if(x > 1){
#				if(y > 1){
#					tmp <- 0;
#				}else{
#					tmp <- exp(-epsilon * y);
#				}
#			}else{
#				tmp <- -exp(-epsilon * x) + exp(-epsilon * y);
#			}
#		}
#	}
#	return  (tmp);
#}

#1: Intervallo (0.005, 0.16]
I_1_1 <- F(0.16) - F(0.005);
I_1_1;
#I_1_2 <- F2(0.16, 0.005); 
#I_1_2;

#2: Intervallo [-0.285, 0.285)
I_2_1 <- F(0.285) - F(-0.285);
I_2_1;
#I_2_2 <- F2(0.285, -0.285);
#I_2_2;

#3: Evento A_1 = (0.252, 0.535] U (0.708, 2.018]
A_1_1 <- F(0.535) - F(0.252) + F(2.018) - F(0.708);
A_1_1;
#A_1_2 <- F2(0.535, 0.252) + F2(2.018, 0.708);
#A_1_2;

#4: Evento A_2 = (0.252, 0.535] U (0.708, 2.018] U (0.005, 0.16]
A_2_1 <- F(0.535) - F(0.252) + F(2.018) - F(0.708) + F(0.16) - F(0.005);
A_2_1;
#A_2_2 <- F2(0.535, 0.252) + F2(2.018, 0.708) + F2(0.16, 0.005);
#A_2_2;