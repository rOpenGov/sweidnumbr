
context("roin and rpin")

test_that(desc="roin",{
  expect_equal(length(roin(c(1,2))), 2)
  expect_silent(x <- roin(100000))
})

test_that(desc="rpin",{
  expect_equal(length(rpin(c(1,2))), 2)
  expect_silent(x <- rpin(100000))
  expect_silent(x <- rpin(100000, p.male = 0))
  expect_equal(as.numeric(table(pin_sex(x))), 100000)
  expect_silent(x <- rpin(100000, p.coordn = 0))
  expect_equal(as.numeric(table(pin_coordn(x))), 100000)
  expect_silent(x <- rpin(100000, start_date = "2000-01-01", end_date = "2000-01-01"))
  suppressMessages(expect_equal(as.numeric(table(pin_age(x))), 100000))
  expect_equal(as.numeric(substr(x, start = nchar(x), stop = nchar(x))), 
               luhn_algo(x, c(0,0,2,1,2,1,2,1,2,1,2,0)))
})
