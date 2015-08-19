# Giraffe

Giraffe is a package that uses the functionality of 'git2r' 'daff' and
'xtable' to view the changes of a csv file that is in a git
repository.

This is a first draft! So nothing may work as expected

## Installation

```
# install.packages("devtools")
library(devtools)
install_github("trosendal/giraffe")
```

## Example

```
library(giraffe)
repo <- read_sample_repo()
path <- "test.csv"
filename <- tempfile(fileext = ".html")
writeLines(render_tables(repo, path), filename)
utils::browseURL(filename)
```


Licence
-------

GPL2
