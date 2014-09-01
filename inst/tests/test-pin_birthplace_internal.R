
cat("pin_birthplace_internal : ")


test_that(desc="birthplace",{
  expect_equal(pin_birthplace_internal(pin = "199000000100", birth_vector = as.character(1:100)), expected = "Born after 31 december 1989")
  expect_equal(pin_birthplace_internal(pin = "194000000100", birth_vector = as.character(1:100)), expected = "2")
  expect_equal(pin_birthplace_internal(pin = "194000009900", birth_vector = as.character(1:100)), expected = "100")
})

cat("\n")