test_that("multiplication works", {
  x<-c(1:10)
  m<-38.5
  expect_identical(map_mean(x,function(.) .^2),38.5)
})
