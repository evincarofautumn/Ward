void lock (void *) __attribute__ ((ward (deny (locked (0)), grant (locked (0)))));
void unlock (void *) __attribute__ ((ward (need (locked (0)), revoke (locked (0)))));
int requires_lock (void *) __attribute__ ((ward (need (locked (0)))));
int denies_lock (void *) __attribute__ ((ward (deny (locked (0)))));

int main ()
{
	void *p;
	lock (p);
	lock (p);
	denies_lock (p);
	unlock (p);
	requires_lock (p);
}
