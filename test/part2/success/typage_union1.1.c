
union test
{
    int i;
    char c;
};

int main(){
	union test t;
	int i;
	char c;

	i = t.i;
	c = t.c;
}
