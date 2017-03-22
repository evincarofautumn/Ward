int foo(void) __attribute__ ((permission (need (explicit))));
int bar(void) __attribute__ ((permission (need (implied))));

int foo (void)
{
	return 0;
}

int bar (void)
{
	return foo ();
}
