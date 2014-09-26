
cat("pin_age : ")

pin_test <- c("198111210000", "196408233234", "198111810000", "196408833234")
today_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),"0000",sep="")

test_that(desc="age",{
  expect_equal(pin_age(pin = pin_test, date = "2012-01-01"), expected = c(30, 47, 30, 47))
  expect_equal(pin_age(pin = today_pin), expected = 0)
  expect_is(pin_age(pin = pin_test, date = "2012-01-01"), "integer")
})

cat("\n")