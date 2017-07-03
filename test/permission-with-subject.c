void lock (void *) __attribute__ ((ward (grant (locked (0)))));
void unlock (void *) __attribute__ ((ward (need (locked (0)), revoke (locked (0)))));
int requires_lock (void *) __attribute__ ((ward (need (locked (0)))));

int main ()
{
	void *p, *q;
	lock (p);
	requires_lock (p); /* ok */
	requires_lock (q); /* no */
	unlock (p);
	requires_lock (p); /* no */
}
