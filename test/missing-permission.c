int foo(void) __attribute__ ((permission (needs (baz))));
int bar(void);

int foo (void)
{
	return 0;
}

int bar (void)
{
	return foo ();
}
