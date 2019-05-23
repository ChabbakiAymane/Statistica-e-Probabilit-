#3. Si consideri X~BiNe(r, p). Qual è la probabilità che Jane passi l'esame dopo x domande

r <- 17
x <- 17
p <- 0.499
q <- 1-p #probabilità di sbagliare la domanda
q
Pr_passa_in_x_domande <- choose(x-1, r-1)*((p)^r)*((q)^(x-r))
Pr_passa_in_x_domande

#4. Qual è la probabilità che superi l'esame dopo y domande

y <- 25

Pr_passa_in_y_domande <- choose(y-1, r-1)*((p)^r)*((q)^(y-r))
Pr_passa_in_y_domande

#5. Calcolare P(X <= n). useremo due metodi: uno con la binomiale negativa mentre l'altro con la binomiale Z~Bi(n,p)

n <- 20.05
n <- trunc(n)

i<-r
Pr_X_minore_uguale_BiNe <- 0

while(i<=(n)){
    Pr_X_minore_uguale_BiNe <- Pr_X_minore_uguale_BiNe + choose(i-1, r-1)*((p)^r)*((q)^(i-r))
    i <- i+1
}
Pr_X_minore_uguale_BiNe

i<-0
Pr_Z_minore_uguale_Bi <- 0

while(i<r){
    Pr_Z_minore_uguale_Bi <- Pr_Z_minore_uguale_Bi + choose(n, i)*((p)^i)*((q)^(n-i))
    i <- i+1
}
Pr_Z_maggiore_uguale_Bi <- 1 -Pr_Z_minore_uguale_Bi
Pr_Z_maggiore_uguale_Bi


#1: TRUE
#2: c(17,0.725)
#3: 7.374104e-06 == (0.499)^17
#4: 0.02152666 == choose(24,16) * (0.499)^17 * (0.501)^8
#5: 0.001251926 == choose(16,16) * (0.499)^17 * (0.501)^0 + choose(17,16) * (0.499)^17 * (0.501)^1 + choose(18,16) * (0.499)^17 * (0.501)^2 + choose(19,16) * (0.499)^17 * (0.501)^3