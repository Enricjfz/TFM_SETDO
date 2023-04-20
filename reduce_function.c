#include <stdio.h>
#include <stdlib.h>

int* reduce(int* arr, int n) {
    if (n <= 2) {
        return arr;  // No se puede reducir más el vector
    }
    
    int* reduced_arr = malloc(n * sizeof(int));  // Vector reducido
    int idx = 0;  // Índice actual en el vector reducido
    
    reduced_arr[idx++] = arr[0];  // Añadir el primer elemento
    
    for (int i = 1; i < n - 1; i++) {
        // Si el elemento actual es distinto de sus vecinos, añadirlo al vector reducido
        if (arr[i] != arr[i - 1] && arr[i] != arr[i + 1]) {
            reduced_arr[idx++] = arr[i];
        }
    }
    
    reduced_arr[idx++] = arr[n - 1];  // Añadir el último elemento
    
    // Redimensionar el vector reducido
    reduced_arr = realloc(reduced_arr, idx * sizeof(int));
    
    return reduced_arr;
}

int main() {
    int arr[] = {2, 2, 3, 4, 5, 5};
    int n = sizeof(arr) / sizeof(int);
    
    int* reduced_arr = reduce(arr, n);
    int reduced_n = sizeof(reduced_arr) / sizeof(int);
    
    printf("Vector reducido: ");
    for (int i = 0; i < reduced_n; i++) {
        printf("%d ", reduced_arr[i]);
    }
    printf("\n");
    
    free(reduced_arr);  // Liberar memoria
    
    return 0;
}
