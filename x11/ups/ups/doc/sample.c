struct argst { char *a_name; struct argst *a_next; };
struct argst *listhead = 0;

void stash(name)
char *name;
{
	struct argst *a;
	char *malloc();

	a = (struct argst *)malloc(sizeof(struct argst));
	a->a_name = name;
	a->a_next = listhead;
	listhead = a;
}
	
int main()
{
	stash("foo");
	stash("bar");
}
