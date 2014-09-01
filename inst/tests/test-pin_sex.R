
cat("pin_sex : ")

pin_test <- c("198112189876", "196408233234", "198112180000", "196408230000")
pin_test_res <- as.factor(c("Male", "Male", "Female", "Female"))

test_that(desc="control number",{
  expect_equal(pin_sex(pin = pin_test), expected = pin_test_res)
})

cat("\n")