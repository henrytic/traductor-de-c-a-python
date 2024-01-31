#include <stdio.h>

int main() {
    int num1 = 10;
    int num2 = 5;
    int result = num1 - num2;

    if (result > 0) {
        printf("El resultado es positivo: %d", result);
    } else {
        printf("El resultado es negativo o cero: %d", result);
    }

    return 0;
}
