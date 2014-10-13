
context("rpin.pin")


##########################################################################################
#                                                                                        #
#                                      Preparation                                       #
#                                                                                        #
##########################################################################################

set.seed(3141592)

p10   <- rpin(10)
p100  <- rpin(100)
p1000 <- rpin(1000)
p1001 <- rpin(1001)



##########################################################################################
#                                                                                        #
#                                         Tests                                          #
#                                                                                        #
##########################################################################################

test_that("Reject incorrect input of x", {
  expect_that(rpin.pin("Hello world!"), throws_error())
  expect_that(rpin.pin(list(1)), throws_error())
})


test_that("Return correct class", {
  expect_that(rpin.pin(p10), is_a("pin"))
  expect_that(rpin.pin(p100), is_a("pin"))
  expect_that(rpin.pin(p1000), is_a("pin"))
  expect_that(attr(rpin.pin(p10), "non_personal"), is_true())
})


test_that("Correct length of output", {
  expect_that(length(rpin.pin(p10)), is_equivalent_to(10))
  expect_that(length(rpin.pin(p100)), is_equivalent_to(100))  
  expect_that(length(rpin.pin(p1000)), is_equivalent_to(1000))
})

test_that("Birth days within borders", {
  expect_that(all(as.Date(pin_to_date(rpin.pin(p1000, l_birth = "1985-05-04"))) >= as.Date("1985-05-04")), is_true())
  expect_that(all(as.Date(pin_to_date(rpin.pin(p1000, u_birth = "1985-05-04"))) <= as.Date("1985-05-04")), is_true())
  expect_that(rpin.pin(p1000, l_birth = "2014-01-01", u_birth = "1830-01-01"), throws_error())
})



test_that("Birthday distribution check", {
  expect_that( 
    ks.test(
      as.numeric(as.Date(pin_to_date(p1000))), 
      as.numeric(as.Date(pin_to_date(rpin.pin(p1000)))), 
    )$p.value, 
  is_more_than(.1)
  )
})




test_that("Correct sex distribution", {
  
  expect_that(prop.test(
    rbind(table(pin_sex(p <- rpin(100))), 
          table(pin_sex(rpin(p)))))$p.value,
    is_more_than(.05)
  )
  
  expect_that(prop.test(
    rbind(table(pin_sex(p <- rpin(1000))), 
          table(pin_sex(rpin(p)))))$p.value,
    is_more_than(.05)
  )
  
  expect_that(prop.test(
    rbind(table(pin_sex(p <- rpin(10000))), 
          table(pin_sex(rpin(p)))))$p.value,
    is_more_than(.05)
  )
})


test_that("All values unique if so specified", {
  expect_that(any(duplicated(rpin.pin(p1000, unique = TRUE))), 
              is_false(),
              info = "This test might fail sometimes. It should pass with high probabily but not deterministicly! Run again!")
  expect_that(any(duplicated(rpin.pin(p1000, l_birth = "2013-01-01", u_birth = "2014-12-31", unique = TRUE))), 
              is_false(),
              info = "This test might fail sometimes. It should pass with high probabily but not deterministicly! Run again!")
  expect_that(rpin.pin(p1001, l_birth = Sys.Date(), unique = TRUE), throws_error())
})


