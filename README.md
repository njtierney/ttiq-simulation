# ttiq-simulation

Exploring scenarios around TTIQ

# Contributing to this repo?

If you are contributing to this repo, please create a branch or fork of the project
and contribute via a pull request - this will avoid issues with everyone working on master at the same time.

# Running analyses

## Step 1: Install `capsule`

```r
install.packages("capsule", repos = "https://milesmcbain.r-universe.dev")
```

or

```r
remotes::install_github("milesmcbain/capsule")
```

## Step 2: Reproduce the libraries used 

```r
capsule::reproduce_lib()
```

This recreates all of the R packages used in the analysis on your computer. Importantly, this will not change where your existing R packages are installed. It is just for this repository. So no need to be concerned about this impacting other analyses you run.

## Step 3: Run the target workflow

```r
capsule::run(targets::tar_make())
```

This runs our targets workflow using the R packages specified.

This will check if the targets are written, and if they aren't, it will re-run the necessary ones. In this case, the targets are stored in the `_targets/objects` folder.

Note that you will for most cases need access to data

## Step 4 - ?

If you install some new packages or use a new package, add a `library` call to `packages.R` and then run:

```r
create(dep_source_paths = "./packages.R")
```

To update the lockfile

# Managing package conflicts

To make sure the right package function names are called, and avoid pain, for example, with `dplyr::select`, we use `conflicted` in the `packages.R` file.

This code:

```r
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
```

Means that `filter` and `select` will always be `dplyr::filter` and `dplyr::select`.

In addition, any conflicts that come up will cause an error.

# Running tests

You can run regression tests against the images created by running:

```r
testthat::test_dir("tests/testthat")
```

This will then compare the current plots in targets against a "golden standard"
plots in `_snaps`.

Note that there might be some warnings - these are usually warnings from ggplot
that say something like:

> Removed 6 rows containing missing values (geom_col)

These are typically known cases where we have NA values in an experiment grid
as there might not be valid values for a given combination of parameters.

The testing for these plots are created in `tests/testthat/test-plots.R`

To review existing plots against new ones, run:

```r
testthat::snapshot_review("test-plots")
```

Note that the tests are not run as part of the targets workflow, as it is a
separate step of the workflow.