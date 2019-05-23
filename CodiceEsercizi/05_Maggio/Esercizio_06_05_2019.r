library(MASS);
options(digits=10)

#Dati
mu <- 46.76;
sigmaQuadro <- 3.67;

#1: Sia X~N(mu, sigmaQuadro). Calcolare Pr(46.92 <= X <= 47.07)
    sx <- 46.92
    dx <- 47.07
    
	Pr_intervallo1 <- pnorm(dx, mu, sqrt(sigmaQuadro)) - pnorm(sx, mu, sqrt(sigmaQuadro));
	Pr_intervallo1
    
    #Prova con la standard
    sx_Std <- (sx-mu)/sqrt(sigmaQuadro)
    dx_Std <- (dx-mu)/sqrt(sigmaQuadro)
    Pr_intervallo1_Std <- pnorm(dx_Std, 0, 1) - pnorm(sx_Std, 0, 1);
	Pr_intervallo1_Std
    
    #Senza normalizzare
	integrate(function(x){	return((1/(sqrt(2*pi*3.67))) * exp(-(1/2) * ((x - 46.76)/sqrt(3.67))^2))},46.92,47.07)$value

	#Con normalizzazione
	integrate(function(x){	return((1/(sqrt(2*pi))) * exp(-(1/2) * ((x)^2)))},0.08351932016,0.1618186828)$value

	#Con formula
	pnorm(0.1618186828, 0, 1) - pnorm(0.08351932016, 0, 1);
    
#2: Calcolare Pr(X>44.62)
	a <- 44.62
    a_Std <- (a-mu)/sqrt(sigmaQuadro)
    Pr_maggiore_a_Std <- 1 - pnorm(a_Std, 0, 1)
    Pr_maggiore_a_Std
	
	#Senza normalizzare
	1 - integrate(function(x){	return((1/(sqrt(2*pi*3.67))) * exp(-(1/2) * ((x - 46.76)/sqrt(3.67))^2))},-Inf,44.62)$value

	#Con normalizzazione
	1 - integrate(function(x){	return((1/(sqrt(2*pi))) * exp(-(1/2) * ((x)^2)))},-Inf,-1.1170709071)$value

	#Con formula
	1 - pnorm(-1.1170709071, 0, 1);

#3: Calcolare Pr(|X| <= 46.97) = Pr(-46.97 <= X <= 46.97)
	b <- 46.97
    Pr_valore_assoluto <- pnorm(b, mu, sqrt(sigmaQuadro)) - pnorm(-b, mu, sqrt(sigmaQuadro))
    Pr_valore_assoluto
    
    #Con la standard
    #Per standardizzare si tenga conto che questa va fatta dopo il modulo!!!
    #Pr((-9.78-mu)/sqrt(sigma) <= Z <= (9.78-mu)/sqrt(sigma))
    
    b_dx_Std <- (b-mu)/sqrt(sigmaQuadro)
    b_sx_Std <- (-b-mu)/sqrt(sigmaQuadro)
    Pr_valore_assoluto_Std <- pnorm(b_dx_Std, 0, 1) - pnorm(b_sx_Std, 0, 1)
    Pr_valore_assoluto_Std
	
#RISULTATI:
#1: 0.0309949878
#2: 0.8680179957
#3: 0.5436442719 == pnorm((46.97 - 46.76)/sqrt(3.67), 0, 1) - pnorm((-46.97 - 46.76)/sqrt(3.67), 0, 1)