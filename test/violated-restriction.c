void lock_foo (void)
	__attribute__ ((ward (need (lock_foo), revoke (lock_foo), grant (foo_locked))));

void unlock_foo (void)
	__attribute__ ((ward (need (foo_locked), revoke (foo_locked), grant (lock_foo))));

void lock_bar (void)
	__attribute__ ((ward (need (lock_bar), revoke (lock_bar), grant (bar_locked))));

void unlock_bar (void)
	__attribute__ ((ward (need (bar_locked), revoke (bar_locked), grant (lock_bar))));

void needs_foo_locked (void)
	__attribute__ ((ward (need (foo_locked))));

void needs_bar_locked (void)
	__attribute__ ((ward (need (bar_locked))));

int missing_foo_locked (void);

int missing_foo_locked (void)
{
	needs_foo_locked ();
}

int has_foo_locked (void)
	__attribute__ ((ward (need (lock_foo))));

int has_foo_locked (void)
{
	lock_foo ();
	needs_foo_locked ();
	unlock_foo ();
}

int locks_foo_recursively (void)
	__attribute__ ((ward (need (lock_foo))));

int locks_foo_recursively (void)
{
	lock_foo ();
	lock_foo ();
	needs_foo_locked ();
	unlock_foo ();
	unlock_foo ();
}

int locks_wrong_nesting (void)
	__attribute__ ((ward (need (lock_foo), need (lock_bar))));

int locks_wrong_nesting (void)
{
	lock_bar ();
	lock_foo ();
	needs_foo_locked ();
	needs_bar_locked ();
	unlock_foo ();
	unlock_bar ();
}

int locks_correct_nesting (void)
	__attribute__ ((ward (need (lock_foo), need (lock_bar))));

int locks_correct_nesting (void)
{
	lock_foo ();
	lock_bar ();
	needs_foo_locked ();
	needs_bar_locked ();
	unlock_bar ();
	unlock_foo ();
}
