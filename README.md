ARE212
======

# Section notes

This is a repository for the latest version of the ARE212 section
notes.  Each section has its own directory that contains at least
two core files:

1. A `knitr` document `.Rnw` that compiles to the `.pdf`, `.tex`,
and `.R` files.  In fact, the org-mode document _is_ the code; and you
can dynamically update all downstream documents from within Emacs'
org-mode.  You do not have to interact with the org-mode document
directly if you are not using Emacs, but rather just with the R code
or PDF write-up.  If you'd like to get set up with Emacs (_highly
recommended_) then please see the next section of this README.

2. A PDF of the section notes, which effectively documents the code.
If you are only interested in following along, rather than running the
code yourself, just browse to the `.pdf` file for the section
(e.g. [`sec-01.pdf`](https://github.com/pbaylis/ARE212/blob/master/section-01/sec-01.pdf))
and click "view raw".  The PDF will begin downloading immediately.

3. An R script that compiles all of the code within the PDF.  Note
that there is no documentation within the code.  Instead, the code is
documented from the PDF description.

If there are supporting images or TeX fragments for the write-ups,
there will be a subdirectory called `inserts/` within the section
header.

The sections are organized as follows:

[`section-01`](https://github.com/pbaylis/ARE212/tree/master/section-01) Preliminaries and setup

[`section-02`](https://github.com/pbaylis/ARE212/tree/master/section-02) Matrix operations in R

[`section-03`](https://github.com/pbaylis/ARE212/tree/master/section-03) OLS regressions from first principles

[`section-04`](https://github.com/pbaylis/ARE212/tree/master/section-04) Goodness of fit

[`section-05`](https://github.com/pbaylis/ARE212/tree/master/section-05) Hypothesis testing

[`section-06`](https://github.com/pbaylis/ARE212/tree/master/section-06) Returns to education, empirical example

[`section-07`](https://github.com/pbaylis/ARE212/tree/master/section-07) Efficiency of GLS and `ggplot2`

[`section-08`](https://github.com/pbaylis/ARE212/tree/master/section-08) Instrumental Variables

[`section-09`](https://github.com/pbaylis/ARE212/tree/master/section-09) Testing for heteroskedasticity

[`section-10`](https://github.com/pbaylis/ARE212/tree/master/section-10) Feasible generalized least squares

[`section-11`](https://github.com/pbaylis/ARE212/tree/master/section-11) Serial correlation

[`section-12`](https://github.com/pbaylis/ARE212/tree/master/section-12) Instrumental variables

[`section-13`](https://github.com/pbaylis/ARE212/tree/master/section-13) Spatial analysis in `R`

[`section-14`](https://github.com/pbaylis/ARE212/tree/master/section-14) Web scraping

## Help me write this!  

This project can and _should_ be treated like any other open source,
collaborative coding project.  If you are interested in helping me
make this project better, [fork the
repo](https://help.github.com/articles/fork-a-repo), edit the screwy
files, and [send a pull
request](https://help.github.com/articles/using-pull-requests).  I
will review and merge the changes -- until someone else takes over!

## Org mode notes

If you are running [Emacs](http://www.gnu.org/software/emacs), then
you have access to [org-mode](http://orgmode.org), an open source
solution for interactive coding and reproducible research.  The code,
documentation, and results are all bundled into the same file.  The
`#+RESULTS` output is automatically generated from the immediately
preceding code block.

![](http://i.imgur.com/CjpeA.png)

A few things to note.  When you try to compile the `.org` files to
a PDF document, you may have to compile it twice or reload the buffer
using `C-u M-x org-reload`.  To tangle the code within the org-mode
document to an `.R` script, you can use the key binding `C-c C-v t`.

You can highlight code by using the `minted` package in LaTeX.  For
this, from the command line, make sure that you invoke `pdflatex` with
the `-shell-escape` flag.  For example,

```bash
cd ~/Dropbox/github/danhamer/ARE212/section-04
pdflatex -shell-escape sec-04
```

Ensure that you have the proper
[`minted.sty`](http://www.ctan.org/pkg/minted) file by downloading the
zipfile, installing it, and then ensuring that LaTeX knows where
everything is:

```bash
unzip minted.zip
cd minted/
latex minted.ins
sudo texhash
```

Finally, you will have to add the following to your `.emacs.d/init.el`
file, and make sure it doesn't conflict with anything else in there:

```lisp
(require 'org-latex)
(setq org-export-latex-listings 'minted)
(add-to-list 'org-export-latex-packages-alist '("" "minted"))
```
