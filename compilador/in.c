#include <stdio.h>

int main() {
    int x = 10;
    int y = 20;
    int z = x + y;
    printf("El resultado de la suma es: %d\n", z);
    if (x > y) {
        printf("x es mayor que y\n");
    else // Error de sintaxis: falta el corchete de cierre
        printf("y es mayor que x\n");
    }
    return 0;
}
