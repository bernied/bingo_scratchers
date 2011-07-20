#include "stdio.h"
#include "stdlib.h"

unsigned int
count_bits(unsigned int v)
{
	unsigned int c;
	v = v - ((v >> 1) & 0x55555555);    // reuse input as temporary
	v = (v & 0x33333333) + ((v >> 2) & 0x33333333);     // temp
	c = ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24; // count

	return c;
}

unsigned int
generate_lower_binomial(unsigned int n, unsigned int k)
{
	unsigned int b = 0;
	for (int i=0; i < k; i++) {
		b <<= 1;
		b |= 1;
	}
	return b;
}

unsigned int
generate_higher_binomial(unsigned int n, unsigned int k)
{
	unsigned int b = generate_lower_binomial(n, k);
	b <<= (n-k);
	return b;
}

void
generate_choose_n_k(unsigned int n, unsigned int k)
{
	unsigned int lower = generate_lower_binomial(n, k);
	unsigned int higher = generate_higher_binomial(n, k);

	for (unsigned int i = lower; i <= higher; i++)
	{
		if (count_bits(i) == k) {
			printf("%i\n", i);
		}	
	}
}

int main(int argc, char** argv)
{
	unsigned int n, k;
	if (argc != 3)
	{
		printf("Expecting two integer arguments.\n");
		exit(-1);
	}

	n = atoi(argv[1]);
	k = atoi(argv[2]);

	if (k > n) {
		printf("k(%i) must be smaller then n(%i)\n", k, n);
		exit(-1);
	}
	if (n > 32) {
		printf("n is larger then 32!\n");
		exit(-1);
	}

	generate_choose_n_k(n, k);

	exit(0);
}
