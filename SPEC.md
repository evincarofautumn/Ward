# Specification

* *p*, *q* &mdash; permissions

* *f*, *g* &mdash; functions

  * *f*() &mdash; a call to *f*

  * *f*(); *g*() &mdash; a call to *f* followed by a call to *g*

* &Delta; &mdash; global declaration context

  * &Delta;, *p* &mdash; permission *p* is declared
  
  * &Delta;, *p* &rarr; *q* &mdash; permission *p* has restrictions *q*

  * &Delta;, **need**(*f*, *p*) &mdash; function *f* is annotated with **need**(*p*)

  * &Delta;, **use**(*f*, *p*) &mdash; function *f* is annotated with **use**(*p*)

  * &Delta;, **grant**(*f*, *p*) &mdash; function *f* is annotated with **grant**(*p*)

  * &Delta;, **revoke**(*f*, *p*) &mdash; function *f* is annotated with **revoke**(*p*)

  * &Delta;, **waive**(*f*, *p*) &mdash; function *f* is annotated with **waive**(*p*)

  * &Delta;, **implicit**(*p*) &mdash; permission *p* is declared implicit

* &Gamma;, &Theta;, &Epsilon; &mdash; permission contexts

  * &Gamma;, *p* &mdash; permission label *p* is in the context

* &Gamma; &vdash; *f* &dashv; &Theta; &mdash; with input context &Gamma;, function call *f* produces output context &Theta;

## Inference Rules

### Declaration Contexts

> ----
>
> &Delta;, *a* &vdash; *a*

### Permission Contexts

Duplicate permissions are irrelevant. (Contraction)

> &Gamma;, *p*, *p* &vdash; *f*() &dashv; &Theta;
>
> ----
>
> &Gamma;, *p* &vdash; *f*() &dashv; &Theta;

Contexts are unordered. (Exchange)

> &Gamma;, *p*, *q* &vdash; *f*() &dashv; &Theta;
>
> ----
>
> &Gamma;, *q*, *p* &vdash; *f*() &dashv; &Theta;

Sequencing composes judgements. (Cut)

> &Gamma; &vdash; *f*() &dashv; &Theta;
>
> &Theta; &vdash; *g*() &dashv; &Epsilon;
>
> ----
>
> &Gamma; &vdash; *f*(); *g*() &dashv; &Epsilon;

### Implicits and Waivers

A permission declared implicit is available unless waived.

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

> &Delta; &vdash; **need**(f, p)
>
> ----
>
> &Gamma;, p &vdash; *f*() &dashv; &Gamma;, p

### Uses

> &Delta; &vdash; **use**(f, p)
>
> &Delta; &vdash; p &rarr; q
>
> ----
>
> &Gamma;, p, q &vdash; *f*() &dashv; &Gamma;, p, q

### Grants

> &Delta; &vdash; **grant**(f, p)
>
> ----
>
> &Gamma; &vdash; *f*() &dashv; &Gamma;, p

### Revocations

> &Delta; &vdash; **revoke**(*f*, *p*)
>
> ----
>
> &Gamma;, *p* &vdash; *f*() &dashv; &Gamma;
