source(here::here("packages.R"))

## Load all R files in R/ folder
lapply(dir_ls(here::here("R")), source)

pois_samples <- rpois(n = 50, lambda = 1)
pois_samples
dist_fun_zeros <- create_empirical_dist(pois_samples, 
                                        fraction_extra_zeros = 0.5)
dist_fun_zeros

dist_fun <- create_empirical_dist(pois_samples)
dist_fun

# we get different densities - which we expect
test_that("density works appropriately on the create_empirical_dist",{
  expect_false(
    density(dist_fun_zeros, 0) == density(dist_fun, 0)
  )
  expect_equal(
    object = density(dist_fun_zeros, 0),
    expected = mix_in_zeros(pmf = density(dist_fun, 0), 
                          fraction_extra_zeros = 0.5)
  )
})

# we get the same density at 0

