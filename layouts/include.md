This file is include.md
=======================

The content of this file is processed with the markdown package, which transforms it into HTML. Note that this is different from **R** markdown!! 

Again, **.Rmd != .md**. 

If you want to write in RMarkdown I suggest knitting to html and using `includeHTML()`. 

## Including code

.md has a lot of similarities to .Rmd, so you can do the usual things like include code inline with backticks, as in `sum(1:10)`.

```{r}
# Some example code
x <- 12
x + 1
```