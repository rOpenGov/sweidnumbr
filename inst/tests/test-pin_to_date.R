
cat("pin_to_date : ")

test_that(desc="pin_to_date",{
  expect_equal(pin_to_date(pin = c("196408233234", "186408833224")), expected = lubridate::ymd(c("1964-08-23","1864-08-23")))
  expect_is(pin_to_date(pin = pin_test), "POSIXct")
})

cat("\n")
