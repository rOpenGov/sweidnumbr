
cat("pin_convert : ")

test_that(desc="pin_convert",{
  expect_equal(sweidnumbr:::pin_convert(pin = "198112189876"), expected = "198112189876")
  expect_equal(sweidnumbr:::pin_convert(pin = "189876"), expected = as.character(NA))
  expect_equal(sweidnumbr:::pin_convert(pin = "811218-9876"), expected = "198112189876")
  expect_equal(sweidnumbr:::pin_convert(pin = "19811218-9876"), expected = "198112189876")
  expect_equal(sweidnumbr:::pin_convert(pin = "8112189876"), expected = "198112189876")
  expect_equal(sweidnumbr:::pin_convert(pin = "001218-0000"), expected = "200012180000")
})

cat("\n")