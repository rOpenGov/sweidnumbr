
context("rpin.numeric")


##########################################################################################
#                                                                                        #
#                                      Preparation                                       #
#                                                                                        #
##########################################################################################

set.seed(3141592)


##########################################################################################
#                                                                                        #
#                                         Tests                                          #
#                                                                                        #
##########################################################################################

test_that("Reject incorrect input of x", {
  expect_that(rpin.numeric("Hello world!"), throws_error())
  expect_that(rpin.numeric(list(1)), throws_error())
})

test_that("Return correct class", {
  expect_that(rpin.numeric(12), is_a("pin"))
  expect_that(rpin.numeric(1.2), is_a("pin"))
  expect_that(rpin.numeric(191212121212), is_a("pin"))
})

test_that("Warning if x is decimal number", {
  expect_that(rpin.numeric(pi), gives_warning())
})