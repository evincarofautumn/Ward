#include "missing-permission.h"

int foo (void)
{
	return 0;
}

int bar (void)
{
	return foo ();
}
