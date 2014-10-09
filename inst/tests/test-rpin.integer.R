context("rpin.integer")


test_that("Reject incorrect input of x", {
  expect_that(rpin.integer("Hello world!"), throws_error())
  expect_that(rpin.integer(list(1)), throws_error())
})


test_that("Return correct class", {
  expect_that(rpin.integer(12), is_a("pin"))
  expect_that(rpin.integer(100), is_a("pin"))
  expect_that(rpin.integer(1000), is_a("pin"))
  expect_that(attr(rpin.integer(12), "non_personal"), is_true())
})


test_that("Correct length of output", {
  expect_that(length(rpin.integer(1)), is_equivalent_to(1))
  expect_that(length(rpin.integer(100)), is_equivalent_to(100))  
  expect_that(length(rpin.integer(1000)), is_equivalent_to(1000))
})

test_that("Birth days within borders", {
  expect_that(all(as.Date(pin_to_date(rpin.integer(1000))) >= as.Date("1900-01-01")), is_true())
  expect_that(all(as.Date(pin_to_date(rpin.integer(1000))) <= as.Date(Sys.Date())), is_true())
  expect_that(all(as.Date(pin_to_date(rpin.integer(1000, l_birth = "1985-05-04"))) >= as.Date("1985-05-04")), is_true())
  expect_that(all(as.Date(pin_to_date(rpin.integer(1000, u_birth = "1985-05-04"))) <= as.Date("1985-05-04")), is_true())
  expect_that(rpin.integer(12, l_birth = "2014-01-01", u_birth = "1830-01-01"), throws_error())
})

test_that("Birth days uniformly distributed", {
  expect_that( ks.test(
                  as.numeric(as.Date(pin_to_date(rpin.integer(10000)))), 
                  "punif", 
                  as.numeric(as.Date("1900-01-01")),
                  as.numeric(as.Date(Sys.Date()))
                  )$p.value, 
               is_more_than(.05)
               )
})


test_that("All values unique if so specified", {
  expect_that(any(duplicated(rpin.integer(1000, unique = TRUE))), is_false())
  expect_that(any(duplicated(rpin.integer(1000, l_birth = "2014-01-01", u_birth = "2014-12-31", unique = TRUE))), is_false())
  expect_that(rpin.integer(1001, l_birth = Sys.Date(), unique = TRUE), throws_error())
})



test_that("Sex distribution correct", {
  expect_that(prop.test(table(pin_sex(rpin(100))), p = .5)$p.value,
              is_more_than(.05))
  expect_that(prop.test(table(pin_sex(rpin(1000))), p = .5)$p.value,
              is_more_than(.05))
  expect_that(prop.test(table(pin_sex(rpin(10000))), p = .5)$p.value,
              is_more_than(.05))
})

