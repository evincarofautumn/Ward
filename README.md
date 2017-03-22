# Ward

A static analysis tool for C.

Ward accepts C99 source files, annotated with *permissions*, and verifies that permissions are correct for all top-level functions. A permission is a constraint on the context in which some code is allowed to run, and can be used to verify things like locking, signal-safety, program phases (e.g., initialization and shutdown), and memory allocation (e.g., disallowing allocation in performance- or safety-critical code).

C programs tend to have a fair amount of global state, which can produce subtle errors if used incorrectly. Ward is designed to make it easier to use this sort of state correctly. It’s a fairly liberal analysis—the goal is to call attention to suspicious code, not verify everything precisely.

## Building

Ward is written in Haskell using the [`language-c`](https://hackage.haskell.org/package/language-c) package for parsing C99. You need [Stack](https://docs.haskellstack.org/en/stable/README/) to build it.

```
git clone git@github.com:evincarofautumn/Ward.git
cd Ward
stack build
stack exec ward -- --help

SYNOPSIS
    ward <cpp> <path>* [--grant=<perm> | -G<perm>] [-- <flags>]
...
```

`stack install` will place a copy of the executable in `~/.local/bin`.

## Running

Ward accepts a path to a C preprocessor (e.g., `gcc`), a list of paths to C sources, options for granting implicit permissions, and additional flags to pass to the preprocessor. On OS X, you’ll want to pass `-fno-blocks` or `-U__BLOCKS__` because the C parsing library used by Ward [does not yet support Apple’s block extension](https://github.com/visq/language-c/issues/15). A typical invocation looks like this:

```
ward gcc foo.c bar.c --grant=malloc -- -I/some/include/path -U__BLOCKS__
```

Ward should build and run on OS X and Linux; it may work on Windows with a suitable preprocessor installed, but this hasn’t been tested.

## Annotating Your Code

Ward accepts annotations in the form of function-level attributes, which specify permissions and the actions Ward should take with them.

```
__attribute__((permission(action(perm, perm, …), action(perm, perm, …), …)))
```

An action may be any of the following:

| Action      | Effect |
| ----------- | ------ |
| `need(p)`   | When this function is called, `p` must be in the context. |
| `deny(p)`   | When this function is called, `p` must *not* be in the context. |
| `waive(p)`  | If `--grant=p` was specified, this function is *not* granted `p`. |
| `grant(p)`  | After this function is called, `p` is added to the context. |
| `revoke(p)` | After this function is called, `p` is removed from the context. |

Here’s a typical example, which assumes Ward is invoked with `--grant=malloc`:

```
#define PERM(...) __attribute__((permission(__VA_ARGS__)))

void must_not_allocate(void)
  PERM(waive(malloc));

void take_lock(void)
  PERM(need(lock), revoke(lock), grant(locked));

void must_have_lock_held(void)
  PERM(need(locked));

void release_lock(void)
  PERM(need(locked), revoke(locked), grant(lock));
```

These annotations enforce the following properties:

* `must_not_allocate`:

  * `waive(malloc)`: this function cannot call functions that implicitly `need(malloc)`

* `take_lock`:

  * `need(lock)`: if a function takes the lock, it must also be annotated with `need(lock)`
  * `revoke(lock)`: when the lock is held, it can’t be recursively locked
  * `grant(locked)`: a function annotated with `need(locked)` can only be called when the lock is held

* `must_have_lock_held`:

  * `need(locked)`: the lock must have been taken already before entering this function

* `release_lock`:

  * `need(locked)`: the lock must be held in order to release it
  * `revoke(locked)`: when the lock is released, a function annotated with `need(locked)` can no longer be called
  * `grant(lock)`: the lock can be taken again after it is released

By implicitly granting `malloc`, you only need to annotate those functions that are explicitly *not* allowed to allocate. You can do a similar thing with e.g. `--grant=signal_unsafe`—functions are not required to be signal-safe by default, but you want to make sure that no signal-safe function calls a signal-unsafe function!

In this way, Ward is pay-as-you-go: you only need to annotate and verify the specific source files and permissions you’re interested in, and you can define whatever meanings you like for a permission, because it’s just a label.

## Known Deficiencies

### No Indirect Calls

Ward currently does not handle indirect calls. In practice this is not a significant limitation, as the vast majority of calls in typical C code are direct, but it may be addressed in the future.

### Limited Testing

It seems to be usable and effective in practice, but it’s not very well tested. I’m working on adding [a comprehensive test suite](https://github.com/evincarofautumn/Ward/tree/master/test).

### Limited Diagnostics

Permissions are created automatically at their first use, so Ward annotations are somewhat susceptible to typos. I plan to [improve the error reporting in general](https://github.com/evincarofautumn/Ward/issues/3), and support [declaring permissions in a configuration file](https://github.com/evincarofautumn/Ward/issues/1), allowing one permission to imply another—for instance, `waive(signal_unsafe)` will also `waive(malloc)` if `malloc -> signal_unsafe`.

### No Complex Control Flow

It also doesn’t handle non-trivial control flow very well, so you may need to restructure some code in order to make it checkable. For example, Ward cannot verify this, because the `locked` permission is conditional:

```
void foo(bool lock) {
  if (lock) {
    take_lock();
  }
  …
  if (lock) {
    release_lock();
  }
}
```

However, it can verify this:

```
void foo_locked(void) {
  take_lock();
  foo_unlocked();
  release_lock();
}

void foo_unlocked(void) {
  …
}
```

(And this is better coding practice, anyway.)
