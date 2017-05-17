int foo(void) __attribute__ ((ward (need (baz))));
int bar(void);

int foo (void)
{
	return 0;
}

int bar (void)
{
	return foo ();
}
