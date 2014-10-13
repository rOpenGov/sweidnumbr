
context("rpin.default")


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

test_that("Guards for unwanted formats",{
          expect_that(rpin.default("Hej pa dig!"), throws_error())
          expect_that(rpin.default(as.Date(Sys.Date())), throws_error())          
})

test_that("Output format", {
          expect_that(rpin("191212121212"), is_a("pin"))
})