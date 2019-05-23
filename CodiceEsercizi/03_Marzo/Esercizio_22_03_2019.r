library(MASS);
#dati
insieme <- 1:5700;
elementi <- 6;

#1 Possibili sottoinsiemi con tot elementi, nota che 2 sottoinsiemi saranno uguali se contengono gli stessi elementi
totaleInsiemi <- choose(insieme, elementi);
#47508726562220800800
totaleInsiemi;

#2 Possibili sottoinsiemi che contengono almeno un numero pari?
countPari <- 0;
countDispari <- 0;
for(i in insieme){
    if(i %% 2 == 0){
        countPari <- countPari + 1;
    }else{
        countDispari <- countDispari + 1;
    }
}
countPari;
countDispari;

insiemiSoloDispari <- choose(countDispari, 6);
insiemiAlmenoUnPari <- totaleInsiemi - insiemiSoloDispari;
#46768355507474077400
insiemiAlmenoUnPari;

#3 Si estraggono casualemente tot elmenti dall'insieme, fra quelli estratti c'è sicuramente uno pari, probabilità che tutti e 6 siano pari?
insiemi6Pari <-  choose(countPari, elementi)[2];
insiemi6Pari;
#probabilità condizionata
#P(6 pari | almeno uno pari) = P(6 pari intersecato almeno un pari)/P(almeno un pari) = P(6 pari)/P(almeno un pari) = casifavorevoli/casitotali
pTuttiPariConAlmenoUnPari <- insiemi6Pari/insiemiAlmenoUnPari;
fractions(pTuttiPariConAlmenoUnPari);