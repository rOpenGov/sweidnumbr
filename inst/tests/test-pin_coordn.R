
cat("pin_coordn : ")

pin_test <- c("198112189876", "196408233234", "198112789876", "196408833234")
pin_test_res <- c(FALSE, FALSE, TRUE, TRUE)

test_that(desc="control number",{
  expect_is(pin_coordn(pin = pin_test), "logical")
  expect_equal(pin_coordn(pin = pin_test), expected = pin_test_res)
})

cat("\n")