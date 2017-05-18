#include "missing-permissions-conditional.h"

int baz(int);

int foo (void)
{
	return 0;
}

int bar (void)
{
	return 1;
}

int baz (int x)
{
	if (x)
		return foo ();
	else
		return bar ();
}
