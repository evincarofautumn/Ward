void begin_unsafe (void) __attribute__ ((ward (need (safe), revoke (safe))));
void end_unsafe (void) __attribute__ ((ward (grant (safe))));
void need_safety (void) __attribute__ ((ward (need (safe))));

void test (void) __attribute__ ((ward (need (safe))));
