if(!require(pracma)){
    install.packages("pracma",repos = "http://cran.us.r-project.org");
}

#Sia (X,Y) una variabile bivariata uniformemente distribuita nel cerchio A = {(x,y) : x^2 + y^2 ≤ 1}
#Distribuzione e densità costante su A

#FUNZIONI:
	F_distribuzione <- function(x){return(1/pi)}
	F_x <- function(x){return(2 * sqrt(1 - x^2) / pi)}
	F_y <- function(y){return(2 * sqrt(1 - y^2) / pi)}


#1: Scrivere la densità marginale di X nel punto X = 0.93
	val_x <- 0.93
	marginale_x <- F_x(val_x)
	cat("Valore marginale di X nel punto X = 0.93 è", marginale_x, "\n");

#2: Scrivere la densità marginale di Y nel punto Y = 0
	val_y <- 0
	marginale_y <- F_y(val_y)
	cat("Valore marginale di Y nel punto Y = 0 è", marginale_y, "\n");

#3: Le variabili X e Y sono stocasticamente indipendenti?
	#F_x(x) * F_y(y) = Fx,y(x,y) ?
	#2/pi * sqrt(1 - x^2) * 2/pi * sqrt(1 - y^2) = 1/pi
	FALSE

#4: Le variabili X e Y sono linearmente indipendenti?
	#Cov(X,Y) = E(XY) - E(X)E(Y) = 0
	
	#integral2(function(x,y){return(x*y*1/pi)}, function(x){return(-sqrt(1-x^2))}, function(x){return(sqrt(1-x^2))}, function(y){return(-sqrt(1-y^2))}, function(y){return(sqrt(1-y^2))},singular=TRUE,sector = FALSE)$value;
	#integral2(function(x,y){return(x*y*1/pi)}, -1,1,-1,1, sector = FALSE, reltol = 1e-6, abstol = 0, maxlist = 5000, singular = FALSE, vectorized = TRUE)$value;
	val_atteso_XY <- 0;
	val_atteso_X <- integrate(function(x){return(2 * x * sqrt(1 - x^2) / pi)}, -1, 1)$value;
	val_atteso_Y <- integrate(function(y){return(2 * y * sqrt(1 - y^2) / pi)}, -1, 1)$value;
	covarianza <- val_atteso_XY - val_atteso_X * val_atteso_Y
	covarianza == 0
	
#RISULTATI:
	#1: 0.2339957 == 2 * sqrt(1 - (0.93)^2) / pi
	#2: 0.6366198 == 2/pi
	#3: FALSE
	#4: TRUE