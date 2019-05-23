library(MASS);

#Funzione: 
#F(x) = c * (x - 1)^3 se 1 <= x <= 4.5
#     = 0 x <= 0
#     = 1 x >= 4.5

F <- function(x) {
	if(x < 1){
		tmp <- 0;
	}else if(x > 4.5){
		tmp <- 1;
	}else{
		tmp <- val_C * (x - 1)^3;
	}
	return (tmp);
}

#Si determini la costante c > 0 tale per cui F è una funzione di probabilità.
#lim (x->4.5) F(x) = F(4.5)
punto_continuita = 4.5;

val_C <- 1/((punto_continuita - 1)^3);
val_C;
fractions(val_C);

#Qual è la probabilità dell' intervallo (-0.344, 0.06]?
I_1 <- F(0.06) - F(-0.344);
I_1;

#Determinare t in modo che l'intervallo (2,t] abbia probabilità 0.5.
#Pr((2,t]) = F(t) - F(2) = 0.5
Pr_data = 0.5;
val_F_t <- Pr_data + F(2);

# F(t) = val_F_t
# F(t) = val_C * (x - 1) ^ 3

#Quindi si ha che (sqrt(3,x) è la radice cubica di x):
#	(x - 1)^3 = val_F_t / val_C
#	sqrt(3, (x - 1)^3) = sqrt(3, (val_F_t / val_C))
#	x - 1 = sqrt(3, (val_F_t / val_C))
#	x = sqrt(3, (val_F_t / val_C)) + 1

#Visto che non c'è la radice cubica, elevo a 1/3
val_x <- (val_F_t / val_C)^(1/3) + 1;
val_x;
fractions(val_x);
