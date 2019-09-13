context("simulateclusters")


test_that("smoke tests",{
    expect_error(CreateSimulatedClusters(1, 3, 10, 100), NA)
    expect_error(CreateSimulatedClusters(1, 5, 10, 100), NA)
    dat <- CreateSimulatedClusters(1, 5, 10, 100)
    expect_error(SimulateReliability(dat[[1]], .9), NA)
})
