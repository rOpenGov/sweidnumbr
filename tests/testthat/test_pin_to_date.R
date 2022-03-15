context("pin_to_date")

test_that(desc="pin_to_date",{
  expect_warning(pin_to_date(pin = c("196408233234", "186408833224")), expected = lubridate::ymd(c("1964-08-23","1864-08-23")))
})