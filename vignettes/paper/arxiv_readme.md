# How to generate a valid arxiv.org submission

1.  knit the `*.qmd` file (the problem is that this automatically deletes the `*.bbl` file necessary for arxiv submission)

2.  open the `*.tex` file and compile it including `bibTeX` compilation (this creates and leaves the `*.bbl` file)

3.  create a `*.zip` file with the following files:

-   `bsvars_files/`
-   `bsvars.tex`
-   `bsvars.bbl`
-   `bsvars.bib`
-   `bsvars.png`
-   `jss.bst`
-   `jss.cls`
-   `jsslogo.jpg`

4.  upload the `*.zip` file as an arxiv.org submission
