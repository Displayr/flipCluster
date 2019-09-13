context("diagnostics")

test_that("Kalinki Harabasz",{
    set.seed(2)
    clusters <- rep(1:3, c(20, 20, 20))
    data <- data.frame(A = c(1, 2.1, 3)[clusters], B = 1)
    km <- KMeans(data, 2)
    # Tested against legacy Q cluster analysis
    expect_equal(CalinskiHarabasz(data, km$centers, km$cluster)$Calinski.Harabasz, 229.4, tol = .1)

    # By definition these are all 1
    ext <- ExternalIndices(2, km$cluster, km$cluster)
    expect_equal(ext$adjusted.rand, 1)
    expect_equal(ext$Jaccard, 1)
    expect_equal(ext$adjusted.rand, 1)

    orthogonal <- rep(1:2, 30)
    ext <- ExternalIndices(2, km$cluster, orthogonal)
    expect_equal(ext$Fowlkes.Mallows, 0.5116257, tol = 0.00001)
    expect_equal(ext$Jaccard, 0.3430657, tol = 0.00001)
    expect_equal(ext$adjusted.rand, -0.0152963, tol = 0.00001) # Should be near 0

})
