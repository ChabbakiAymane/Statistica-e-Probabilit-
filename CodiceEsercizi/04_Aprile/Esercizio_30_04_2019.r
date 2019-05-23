library(MASS);
#Sia X~N(μ,σ^2)

#Dati
mu <- 2.7;
sigma2 <- 1.21;

#Funzioni normale standard
f_z_2 <- function(z){return (z^2 * (1/sqrt(2*pi)) * exp(-(z^2)/2))}
f_z_4 <- function(z){return (z^4 * (1/sqrt(2*pi)) * exp(-(z^2)/2))}

#1: Quanto vale il secondo momento non centrato della variabile X?
momento_secondo_nc <- mu^2 + sigma2;
momento_secondo_nc;

#Si consideri ora la trasformazione Y = g(X) = ((X-mu)/sigma)^2
#Y = g(x) = (h(x))^2 con h(x) = ((X-mu)/sigma)^2
#Y = Z^2
	
#2: Qual è la media di Y?
	#E(Y) = E(g(X)) = E(Z^2)
	media <- integrate(f_z_2, -Inf, +Inf, stop.on.error = FALSE)$value;
	media;

#3: Qual'è la varianza di Y?
	#Var(Y) = E(Y^2) - E(Y)^2
	#Var(Y) = E((Z^2)^2) - E(Z^2)^2
	#Var(Y) = E(Z^4) - E(Z^2)^2
	
	varianza <- integrate(f_z_4, -Inf, +Inf, stop.on.error = FALSE)$value - (integrate(f_z_2, -Inf, +Inf, stop.on.error = FALSE)$value)^2;
	varianza;
	
#4: Scrivere la funzione di densità, function(z){...}, della variabile 
#   Z=(X-mu)/sigma, dove X è la variabile del punto 1
	function(z){(1/sqrt(2*pi)) * exp(-(z^2)/2)}
	
#1: 8.5 == (2.7)^2 + 1.21
#2: 1 == integrate(function(z){return (z^2 * (1/sqrt(2*pi)) * exp(-(z^2)/2))}, -Inf, +Inf, stop.on.error = FALSE)$value;
#3: 2 == integrate(function(z){return (z^4 * (1/sqrt(2*pi)) * exp(-(z^2)/2))}, -Inf, +Inf, stop.on.error = FALSE)$value - (integrate(function(z){return (z^2 * (1/sqrt(2*pi)) * exp(-(z^2)/2))}, -Inf, +Inf, stop.on.error = FALSE)$value)^2;
#4: function(z){return((1/sqrt(2*pi)) * exp(-(z^2)/2))}
