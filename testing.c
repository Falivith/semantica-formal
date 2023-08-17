#include <stdio.h>

int main (void){

    int x, y, z, w;

    y = 1;

    x = 3; // Base
    z = 3; // Potencia

    w = x;

    do {
        w = w * x;
        y = y + 1;
    } while (y <= z); 

    printf("\n%d\n", w);

    return 0;
}
