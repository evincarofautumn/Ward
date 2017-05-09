int foo(void) __attribute__ ((permission (denies (baz))));
int bar(void) __attribute__ ((permission (needs (baz))));

int foo (void)
{
	return 0;
}

int bar (void)
{
	return foo ();
}
