# Ward

A static analysis tool for C.

Ward accepts C99 source files, annotated with *permissions*, and verifies that permissions are correct for all top-level functions. A permission is a constraint on the context in which some code is allowed to run, and can be used to verify things like locking, signal-safety, program phases (e.g., initialization and shutdown), and memory allocation (e.g., disallowing allocation in performance- or safety-critical code).

C programs tend to have a fair amount of global state, which can produce subtle errors if used incorrectly. Ward is designed to make it easier to use this sort of state correctly. It’s a fairly liberal analysis—the goal is to call attention to suspicious code, not verify everything precisely.

## Building

[![Build Status](https://travis-ci.org/evincarofautumn/Ward.svg?branch=master)](https://travis-ci.org/evincarofautumn/Ward)

Ward is written in Haskell using the [`language-c`](https://hackage.haskell.org/package/language-c) package for parsing C99. You need [Stack](https://docs.haskellstack.org/en/stable/README/) to build it.

```
> git clone git@github.com:evincarofautumn/Ward.git
> cd Ward
> stack setup  # if this is your first time using Stack
> stack build
> stack exec ward -- --help

Ward - A static analysis tool for C.

Usage: ward CPP [-M|--mode html|compiler|graph] [-C|--config FILE] [-q|--quiet]
            PATH... [-P|--preprocess FLAG]
  Verify Ward permissions for all top-level functions.

Available options:
  CPP                      Name of preprocessor.
  -M,--mode html|compiler|graph
                           Output mode style (default 'compiler'). The 'graph'
                           mode does not run analyses, just parses the source
                           files and emits inferred call graph.
  -C,--config FILE         Read permission information from configuration FILE.
  -q,--quiet               Suppress output except for errors.
  PATH...                  Paths to C source files.
  -P,--preprocess FLAG     Pass FLAG to preprocessor.
  -h,--help                Show this help text
```

`stack install` will place a copy of the executable in `~/.local/bin` (Mac & Linux) or `%APPDATA%\local\bin` (Windows).

## Running

Ward accepts a path to a C preprocessor (e.g., `gcc`), a list of paths to C sources, one or more config files, and additional flags to pass to the preprocessor. On OS X, you’ll want to pass `-fno-blocks` or `-U__BLOCKS__` because the C parsing library used by Ward [does not yet support Apple’s block extension](https://github.com/visq/language-c/issues/15). A typical invocation looks like this:

```
ward gcc foo.c bar.c --config=my.config -- -I/some/include/path -U__BLOCKS__
```

Ward should build and run on OS X and Linux; it may work on Windows with a suitable preprocessor installed, but this hasn’t been tested.

## Writing a Config File

Ward determines how to carry out its analysis based on *config files* with the following format. You can check out the [grammar of config files](https://github.com/evincarofautumn/Ward/blob/b14945c1f3d7b1dc9b0e387fbcef923790f58541/src/Config.hs#L18) for more details.


### Comments

```
// This is a comment. It begins with two slashes and continues to the end of the line.
```

### Declarations and Restrictions

In order to use a permission, you must declare it in the config by specifying its name.

```
my_permission;
```

A permission may be annotated with a *description* string for documentation purposes and to help produce better error messages.

```
my_permission "permission to do the thing";
```

After the permission name and before the description (if present), you can specify *modifiers* for the permission. Currently the only modifier is `implicit`:

```
alloc implicit "can allocate";
```

With the `implicit` modifier, the `alloc` permission will be implicitly granted to all functions unless they explicitly waive it (see “Annotating Your Code”).

Permissions may have any number of additional *restrictions*, which consist of an ASCII rightwards arrow `->` followed by a Boolean expression using the operators `&` (and), `|` (or), and `!` (not). Restrictions specify that if the permission to the left of the arrow is used, then the context must have or lack some additional permissions specified on the right. Like permissions, restrictions can also have description strings.

```
lock_held       "assume the lock is held";

take_lock       "take the lock"
  -> !lock_held "cannot take the lock recursively";
```

### Enforcements

Ward will only enforce annotations for functions specified in the config, so you can add Ward annotations incrementally to an existing codebase. Enforcement directives begin with `.enforce` and accept:

 * A file name, to enforce annotations for all functions declared or defined in that file;
 * A function name, to enforce annotations for a particular function; or
 * Both a file name and function name, to enforce annotations for a single function if defined in the given file—this is useful for `static` functions.

```
.enforce "locks.h";                    // All functions declared in a path ending in "locks.h"
.enforce foo_bar;                      // All functions named "foo_bar"
.enforce "locks-internal.c" do_stuff;  // The function "do_stuff" in "locks-internal.c"
```

## Annotating Your Code

Ward accepts annotations in the form of function-level attributes, which specify permissions and the actions Ward should take with them.

```
__attribute__((ward(action(perm, perm, …), action(perm, perm, …), …)))
```

An *action* may be any of the following:

| Action      | Effect |
| ----------- | ------ |
| `need(p)`   | When this function is called, `p` must be in the context. |
| `use(p)`    | Same as `need`, but expresses the intent that `p` is used directly, and should be subject to restrictions specified in the config. |
| `deny(p)`   | When this function is called, `p` must *not* be in the context. |
| `waive(p)`  | If `p` was declared `implicit`, this function is *not* granted `p`. |
| `grant(p)`  | After this function is called, `p` is added to the context. |
| `revoke(p)` | After this function is called, `p` is removed from the context. |

Here’s a typical example, where `alloc` is declared `implicit`:

```
#define PERM(...) __attribute__((ward(__VA_ARGS__)))

void must_not_allocate(void)
  PERM(waive(alloc));

void take_lock(void)
  PERM(need(lock), revoke(lock), grant(locked));

void must_have_lock_held(void)
  PERM(need(locked));

void release_lock(void)
  PERM(need(locked), revoke(locked), grant(lock));
```

These annotations enforce the following properties:

* `must_not_allocate`:

  * `waive(alloc)`: this function cannot call functions that implicitly `need(alloc)`

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

By implicitly granting `alloc`, you only need to annotate those functions that are explicitly *not* allowed to allocate. You can do a similar thing with e.g. `signal_unsafe implicit;`—functions are not required to be signal-safe by default, but you want to make sure that no signal-safe function calls a signal-unsafe function!

In this way, Ward is pay-as-you-go: you only need to annotate and verify the specific source files and permissions you’re interested in, and you can define whatever meanings you like for a permission, because it’s just a label.

## Known Deficiencies

### Poor Performance

On a project of nontrivial size (dozens of kloc), Ward consumes a large amount of memory and operates slowly due to retaining a large amount of `language-c` AST information. The preferred method for checking a large project is to build a *call map* for each translation unit *separately* using `--mode=graph`, and then load & check all the call maps together. You can see an example of this in [`check-mono.sh`](https://github.com/evincarofautumn/Ward/blob/master/check-mono.sh).

### No Indirect Calls

Ward currently does not handle indirect calls. In practice this is not a significant limitation, as the vast majority of calls in typical C code are direct, but it may be addressed in the future.

### Limited Testing

It seems to be usable and effective in practice, but it’s not very well tested. We’re working on adding [a comprehensive test suite](https://github.com/evincarofautumn/Ward/tree/master/test).

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

Ward doesn’t know whether the `if` branch will be taken, so it conservatively assumes that it requires the permission to lock. This may be addressed in the future to support more coding patterns. However, as a workaround, currently it can verify this alternative formulation:

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

(And this is arguably better coding practice, anyway.)
