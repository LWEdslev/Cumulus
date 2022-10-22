/*
multiline comment
*/

{
    fun isPrime(n) = n > 1 & {
        let i = n-1 ;
        let out = true;
        loop if (i > 2) {
          out = if (n % i == 0) {false} else {out};
          i = i - 1
        };
        out
    };
    let p = [];
    let j = 0;
    loop if(j < 10) {
        p = if (isPrime(j)) {
            p + [j]
        } else {
            p
        };
        j = j + 1
    };
    p(4) #p is now a list with primes so here we get the fourth prime, cumulus is one-indexed
}