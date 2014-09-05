
cat("pin_ctrl : ")

pin_test <- c("196408233234", "196408233235")
pin_test_res <- c(TRUE, FALSE)

test_that(desc="control number",{
  expect_is(pin_ctrl(pin = pin_test), "logical")
  expect_equal(pin_ctrl(pin = pin_test), expected = pin_test_res)
})

cat("\n")