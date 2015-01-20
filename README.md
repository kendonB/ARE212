ARE212
======

# Section notes

This is a repository for the latest version of the ARE212 section
notes.  Each section has its own directory that contains at least
three core files:

1. A `knitr` document `.Rnw` that compiles to the `.pdf`,
and `.R` files.  In fact, the `knitr` document _is_ the code; and you
can dynamically update all downstream documents from within any text editor
that can compile `.Rnw` files (I use TexStudio).  
You do not have to interact with the `.Rnw` document
directly, but rather just with the R code
or PDF write-up.

2. A PDF of the section notes, which effectively documents the code.
If you are only interested in following along, rather than running the
code yourself, just browse to the `.pdf` file for the section
(e.g. [`section-01.pdf`](https://github.com/kendonB/ARE212/blob/master/section-01/section-01.pdf))
and click "view raw".  The PDF will begin downloading immediately.

3. An R script that compiles all of the code within the PDF.  Note
that there is little documentation within the code.  Instead, the code is
documented from the PDF description.

