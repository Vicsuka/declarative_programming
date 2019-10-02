#include "cekla.h"

int osszekevert(const int S, const int A) {
    if (S == 0) return S;
    if (A == 0) return 0;
    if (A == 1) return 0;
    if (A > 10) return S;
    return toDecimal(
        reverse(
            convert(S,A),
            0,
            setMode(getDigits(convert(S,A),0)),
            0,
            getDigits(convert(S,A),0),
            switcher(setMode(getDigits(convert(S,A),0)))
            ),
        0,
        A
    );

}

int getDigits(int n,int digits) {
    if (n/10 > 0)
        return getDigits(n/10,digits+1);
    else
        return digits+1;
}

int setMode(int n) {
    if (n%2 == 0)
        return 1;
    else
        return 0;
}

//Ötlet forrás:https://stackoverflow.com/questions/10375922/base-number-converter-from-10-base-with-recursive-function-c
int convert(int number,int base){
    if(number == 0)
        return number;
    if(base==10)
        return number;

    return (number % base) + 10*convert(number / base, base);
}

int reverse(int n,int rev,int allow,int depth,int digits,int startedWith) {
    if(n == 0)
        return rev; 
    if (allow == 1)
        return reverse(n / 10, rev + (n % 10) * power(10,digits-2+startedWith), 0, depth + 1, digits - 1, startedWith);
    else 
        return reverse(n / 10, rev + (n % 10) * power(10,depth), 1, depth + 1, digits - 1, startedWith);
}

int power(int base, int powerRaised){
    if (powerRaised != 0)
        return (base*power(base, powerRaised-1));
    else
        return 1;
}

//Ötlet forrás:https://stackoverflow.com/questions/26701548/recursion-binary-to-decimal
int toDecimal(int n, int p,int b) {
    if(n == 0) return 0;
    if(p == 0) return n%10 + toDecimal(n/10, b, b);
    return  (n%10) * p +  toDecimal(n/10, b*p, b);
}

int switcher(int i){
    if (i == 1) return 0;
    else return 1;
}