[![Build Status](https://travis-ci.org/trosendal/giraffe.svg)](https://travis-ci.org/trosendal/giraffe)

# Giraffe

Giraffe is a package that uses the functionality of
'[git2r](https://github.com/ropensci/git2r)'
'[daff](https://github.com/edwindj/daff)' and
'[xtable](http://r-forge.r-project.org/projects/xtable/)' to view the
changes of a csv file that is in a git repository.

This is a first draft! So nothing may work as expected

## Installation

```
# install.packages("devtools")
library(devtools)
install_github("trosendal/giraffe")
```

## Example

The function takes a git repository object as defined in 'git2r' and a
relative path to the file of interest in the repository. In the
example below the file in named: "test.csv" and is at the root of the
sample repository that is bundled with the package.

```
library(giraffe)
repo <- read_sample_repo()
path <- "test.csv"
render_tables(repo, path)
```

The default behaviour is to pop a browser window and view the .html. If
you set view = FALSE then you only get an invisible() object back

If you want to submit another repository to the function then you
need to read it in with 'git2r' like this:

```
library(git2r)
repo <- repository("path to your repo")
```

Licence
-------

GPL2
