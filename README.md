# ttiq-simulation

Exploring scenarios around TTIQ

# Running analyses

## Step 1: Install `capsule`

```r
install.packages("capsule", repos = "https://milesmcbain.r-universe.dev")
```

## Step 2: Reproduce the libraries used 

```r
capsule::reproduce_lib()
```

This recreates all of the R packages used in the analysis on your computer. Importantly, this will not change where your existing R packages are installed. It is just for this repository. So no need to be concerned about this impacting other analyses you run.

## Step 3: Run the target workflow

```r
capsule::run(targets::tar_make(<TARGERT>))
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

# Managing conflicts

To make sure the right package function names are called, and avoid pain, for example, with `dplyr::select`, we use `conflicted` in the `packages.R` file.

This code:

```r
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
```

Means that `filter` and `select` will always be `dplyr::filter` and `dplyr::select`.

In addition, any conflicts that come up will cause an error.