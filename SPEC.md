# Specification

## Notation

* *p*, *q* &mdash; permissions

* *e* &mdash; expressions or statements (termed &ldquo;call trees&rdquo; in the
  implementation)

  * *f*, *g* &mdash; functions

  * *f*() &mdash; a call to *f*

  * *e*<sub>1</sub>; *e*<sub>2</sub> &mdash; the term *e*<sub>1</sub> separated
    from *e*<sub>2</sub> by a sequence point

  * *e*<sub>1</sub> | *e*<sub>2</sub> &mdash; the term *e*<sub>1</sub> in a
    parallel conditional branch with *e*<sub>2</sub>

* &Delta; &mdash; global declaration context

  * &Delta;, *p* &mdash; permission *p* is declared
  
  * &Delta;, *p* &rarr; *q* &mdash; permission *p* has restrictions *q*

  * &Delta;, **need**(*f*, *p*) &mdash; function *f* is annotated with
    **need**(*p*)

  * &Delta;, **use**(*f*, *p*) &mdash; function *f* is annotated with
    **use**(*p*)

  * &Delta;, **grant**(*f*, *p*) &mdash; function *f* is annotated with
    **grant**(*p*)

  * &Delta;, **revoke**(*f*, *p*) &mdash; function *f* is annotated with
    **revoke**(*p*)

  * &Delta;, **waive**(*f*, *p*) &mdash; function *f* is annotated with
    **waive**(*p*)

  * &Delta;, **implicit**(*p*) &mdash; permission *p* is declared implicit

* &Gamma;, &Theta;, &Epsilon; &mdash; permission contexts

  * &Gamma;, *p* &mdash; permission label *p* is in the context

* &Gamma; &vdash; *f* &dashv; &Theta; &mdash; with input context &Gamma;,
  function call *f* produces output context &Theta;

## Inference Rules

### Declaration Contexts

> ----
>
> &Delta;, *a* &vdash; *a*

### Permission Contexts

Duplicate permissions are irrelevant. (Contraction)

> &Gamma;, *p*, *p* &vdash; *e* &dashv; &Theta;
>
> ----
>
> &Gamma;, *p* &vdash; *e* &dashv; &Theta;

Contexts are unordered. (Exchange)

> &Gamma;, *p*, *q* &vdash; *e* &dashv; &Theta;
>
> ----
>
> &Gamma;, *q*, *p* &vdash; *e* &dashv; &Theta;

Sequencing composes judgements. (Cut)

> &Gamma; &vdash; *e*<sub>1</sub> &dashv; &Theta;
>
> &Theta; &vdash; *e*<sub>2</sub> &dashv; &Epsilon;
>
> ----
>
> &Gamma; &vdash; *e*<sub>1</sub>; *e*<sub>2</sub> &dashv; &Epsilon;

Parallel judgements must be compatible.

> &Gamma; &vdash; *e*<sub>1</sub> &dashv; &Theta;
>
> &Gamma; &vdash; *e*<sub>2</sub> &dashv; &Epsilon;
>
> &Theta; &cong; &Epsilon;
>
> ----
>
> &Gamma; &vdash; *e*<sub>1</sub> | *e*<sub>2</sub> &dashv; &Theta;

### Implicits and Waivers

A permission declared implicit is available to a function unless that function
waives it.

> &Delta; &vdash; **implicit**(*p*)
>
> &Delta; &nvdash; **waive**(*f*, *p*)
>
> &Gamma;, *p* &vdash; *f*() &dashv; &Theta;
>
> ----
>
> &Gamma; &vdash; *f*() &dashv; &Theta;

### Needs

Needing a permission asserts that it&rsquo;s present in the context, without
changing the context.

> &Delta; &vdash; **need**(f, p)
>
> ----
>
> &Gamma;, p &vdash; *f*() &dashv; &Gamma;, p

### Uses

Uses are the same as needs, except that they also impose any implied
restrictions at the call site.

> &Delta; &vdash; **use**(f, p)
>
> &Delta; &vdash; p &rarr; q
>
> ----
>
> &Gamma;, p, q &vdash; *f*() &dashv; &Gamma;, p, q

### Grants

Granting a permission adds it to the context.

> &Delta; &vdash; **grant**(f, p)
>
> ----
>
> &Gamma; &vdash; *f*() &dashv; &Gamma;, p

### Revocations

Revoking a permission removes it from the context.

> &Delta; &vdash; **revoke**(*f*, *p*)
>
> ----
>
> &Gamma;, *p* &vdash; *f*() &dashv; &Gamma;
