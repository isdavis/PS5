context("Calculating Simpson's Rule")
test_that("Simpson's Rule integral approximation is correct", {
  f1<-new("Simpson", x=c(1,3,5), y=c(1,3,1), ab=c(1,5))
  f2<-new("Simpson", x=c(1,3,5), y=c(1,5,1), ab=c(1,5))
  expect_equal(printSimp(f2), 14.666667, tolerance=1e-3)
  expect_equal(printSimp(f1), 9.3333333, tolerance=1e-3)
})

context("Calculating Trapezoid Rule")
test_that("Trapezoid Rule integral approximation is correct", {
  f1<-new("Trapezoid", x=c(1,3,5), y=c(1,3,1), ab=c(1,5))
  f2<-new("Trapezoid", x=c(1,3,5), y=c(1,5,1), ab=c(1,5))
  expect_that(printTrap(f2), equals(12))
  expect_that(printTrap(f1), equals(8))
})

context("Check for Linear Functions")
test_that("The two should both be equal to the definite integral for a linear function", {
  fS<-new("Simpson", x=c(1,3,5), y=c(1,3,5), ab=c(1,5))
  fT<-new("Trapezoid", x=c(1,3,5), y=c(1,3,5), ab=c(1,5))
  expect_that(printSimp(fS), equals(12))
  expect_that(printTrap(fT), equals(12))
})