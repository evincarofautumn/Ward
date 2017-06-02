int foo(void) __attribute__ ((ward (need (baz))));
int bar(void) __attribute__ ((ward (deny (baz))));

int foo (void)
{
	return 0;
}

int bar (void)
{
	return foo ();
}
