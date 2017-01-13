context("Bugs")

test_that("DS-975",
          {
              library(foreign)
              suppressWarnings({
                gss <- read.spss("http://docs.displayr.com/images/7/7b/GSS2014.sav", to.data.frame = TRUE)
                gss <- flipExampleData::TidySPSS(gss)
                attach(gss)
                kmeans <- KMeans(data.frame(confinan, conbus, coneduc, conlabor, conmedic, conpress), show.labels = TRUE)
                expect_error(print(kmeans), NA)
                detach(gss)
              })
          })





