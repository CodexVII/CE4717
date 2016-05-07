#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    double x[20];
    int i;
    for (i = 0; i < 20; i++) x[i] = (double)(i*i)/1000.0;
    for (i = 0; i < 20; i++)
	printf("%2d  %7.5lf\n", i, x[i]);
    return EXIT_SUCCESS;
}

