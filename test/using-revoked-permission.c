#include "using-revoked-permission.h"

void
test (void)
{
	begin_unsafe ();
	need_safety ();
	end_unsafe ();
}
