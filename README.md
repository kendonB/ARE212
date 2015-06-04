ARE212 Spring 2015
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

Please see https://github.com/pbaylis/ARE212 and https://github.com/danhammer/ARE212
for the excellent previous versions of these notes.

# Tips for the next instructor

1. Make sure the restrictions are really clear in the intros to exams. For example,
make sure the Professor explicitly mentions you are allowed to use all of:
  * loop functions (the apply family, for example)
  * logical operators/functions
  * basic arithmetic functions
  * distribution functions
  * random number generators
  * data manipulation functions

2. Use online submissions for problem sets and exams.

It's better.

3. 
