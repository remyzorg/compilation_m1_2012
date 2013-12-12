struct a {
	struct a * x;
};


struct t {
	int b;
};


struct a returnStruct (struct a s) {
	return s;
}

int fact (int n){
	if (n == 1) return 1;
	else 
		return n * fact(n - 1);
}


void print (char * s, int size){
	int i;
	for (i = 0; i < size; i++){
		putchar (*(s + i));
	
	}
}


int main (int argc, char ** argv) {

	struct a y, w;
	struct t z32;
	int a, c, i;

	char * j;
	char testc;

	testc = j;
	
	j = -1;
	
	if (j == -1) {
		print (5, "ahaa");
		print ("ahaa", 5);
	}

	j = j + testc;

	z32.b = 5;
	w = returnStruct(y);

	if (a)
		fact (a);
	
	main (argc, argv);
	if ( a ){
		putchar(5);
	}

	while (c){
		c++;
		if (c == 42) c = 0;
	}


	for (i = 0; i < 34; i++){
		putchar('a');
	}

	
}

