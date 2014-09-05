
cat("pin_coordn : ")

pin_test <- c("196408233234", "196408830000")
pin_test_res <- c(FALSE, TRUE)

test_that(desc="control number",{
  expect_is(pin_coordn(pin = pin_test), "logical")
  expect_equal(pin_coordn(pin = pin_test), expected = pin_test_res)
})

cat("\n")