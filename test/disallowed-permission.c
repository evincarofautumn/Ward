int foo(void) __attribute__ ((permission (deny (baz))));
int bar(void) __attribute__ ((permission (need (baz))));

int foo (void)
{
	return 0;
}

int bar (void)
{
	return foo ();
}
