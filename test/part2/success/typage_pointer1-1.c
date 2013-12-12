int main(){
	
	int *i;
	char *c;	
	void * v;	
	
	*i = 5;
	*c = 'c';

	v = i;
	v = c;

	i = v;
	c = v;
}
