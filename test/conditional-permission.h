void lock (void) __attribute__ ((ward (use (lock), revoke (lock), grant (locked))));
void unlock (void) __attribute__ ((ward (use (locked), revoke (locked), grant (lock))));
void need_locked (void) __attribute__ ((ward (need (locked))));

void test (int) __attribute__ ((ward (need (lock))));
