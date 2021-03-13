test_that("multiplication works", {
  fitglm <- blbglm(Species ~ Sepal.Length * Sepal.Width, iris[1:100,], 3, 100, family = binomial, Parallel = FALSE)
  expect_s3_class(fitglm, "blbglm")
  colm <- coef(fitglm)
  expect_equal(length(colm), 4)
})
