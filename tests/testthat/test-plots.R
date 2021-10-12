# to run tests, run:
# testthat::test_dir("tests/testthat")
withr::with_dir(here::here(), {
  targets::tar_load(starts_with("plot_"))
})
test_that("plot_vic_casual_cases_monthly works", {
  vdiffr::expect_doppelganger("plot_vic_casual_cases_monthly",
                              plot_vic_casual_cases_monthly)
})
test_that("plot_nsw_delays_optimal works", {
  vdiffr::expect_doppelganger("plot_nsw_delays_optimal",
                              plot_nsw_delays_optimal)
})
test_that("plot_tti_ecdf_comparison works", {
  vdiffr::expect_doppelganger("plot_tti_ecdf_comparison",
                              plot_tti_ecdf_comparison)
})
test_that("plot_hist_tp_reductions works", {
  vdiffr::expect_doppelganger("plot_hist_tp_reductions",
                              plot_hist_tp_reductions)
})
test_that("plot_scenario_vaccination_isolation works", {
  vdiffr::expect_doppelganger("plot_scenario_vaccination_isolation",
                              plot_scenario_vaccination_isolation)
})
test_that("plot_hist_delay_samples_v_data works", {
  vdiffr::expect_doppelganger("plot_hist_delay_samples_v_data",
                              plot_hist_delay_samples_v_data)
})
test_that("plot_tp_reduction works", {
  vdiffr::expect_doppelganger("plot_tp_reduction",
                              plot_tp_reduction)
})
test_that("plot_tp_reduction_over_prop_zeros works", {
  vdiffr::expect_doppelganger("plot_tp_reduction_over_prop_zeros",
                              plot_tp_reduction_over_prop_zeros)
})

# test_that("plot_scenario_vaccination_isolation_unfaceted works", {
#   vdiffr::expect_doppelganger("plot_scenario_vaccination_isolation_unfaceted",
#                               plot_scenario_vaccination_isolation_unfaceted)
# })
# 
