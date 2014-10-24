
cat("pin_age : ")

pin_test <- c("198111210000", "196408233234", "198111810000", "196408833234")
today_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),"0000",sep="")

test_that(desc="age",{
  expect_equal(suppressMessages(pin_age(pin = pin_test, date = "2012-01-01")), expected = c(30, 47, 30, 47))
  expect_equal(suppressMessages(pin_age(pin = today_pin)), expected = 0)
  expect_is(suppressMessages(pin_age(pin = pin_test, date = "2012-01-01")), "integer")
})


test_that(desc="age in years at leapyear",{
  skip_on_travis()
  skip("Test fails currently due to lubridate error/bug.")
  expect_equal(pin_age(pin = c("200002291234", "200002281234"), date = "2012-01-01"), expected = c(14, 14))
})

test_that(desc="age at leapyear",{
  expect_equal(suppressMessages(pin_age(pin = c("200002281234"), date = "2000-05-01", timespan = "months")), expected = 2)
  expect_equal(suppressMessages(pin_age(pin = c("200002011234"), date = "2000-02-08", timespan = "weeks")), expected = 1)
  expect_equal(suppressMessages(pin_age(pin = c("200002011234"), date = "2000-02-07", timespan = "weeks")), expected = 0)
  expect_equal(suppressMessages(pin_age(pin = c("200002011234"), date = "2000-02-07", timespan = "days")), expected = 6)
})

test_that(desc="age in years at leapyear",{
  expect_warning(pin_age(pin = c("200002281234", "200002281234"), date = c("2012-01-01", "2013-01-01")))
  expect_message(pin_age(pin = c("200002281234", "200002281234"), date = c("2012-01-01")))  
})

cat("\n")
