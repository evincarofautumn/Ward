#include "conditional-permission.h"

void
test (int x)
{
	if (x) lock ();
	need_locked ();
	if (x) unlock ();
}
