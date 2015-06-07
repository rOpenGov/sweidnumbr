
context("pin_test_file")

load(file = "test_pin.Rdata")

test_that(desc="class in df",{
  test_pins$pin <- as.pin(test_pins$pin_raw)
  expect_is(test_pins$pin, "pin")
})

test_that(desc="Frequencies in test file",{
  expect_equal(as.numeric(table(pin_sex(test_pins$pin))), c(9299,9328))
  expect_equal(as.numeric(table(pin_age(test_pins$pin, "2015-01-01")))[c(1,2,10,50,99)], c(363,374,364,26,365))  
  expect_equal(as.numeric(table(pin_ctrl(test_pins$pin))), 18627)
  expect_equal(as.numeric(table(pin_coordn(test_pins$pin))), 18627)
  expect_equal(as.numeric(table(pin_birthplace(test_pins$pin)))[c(1,2,10,27:28)], c(5,3,46,7724,9105))  
  expect_equal(as.character(min(unique(pin_to_date(test_pins$pin)))), "1890-01-01")  
  expect_equal(as.character(max(unique(pin_to_date(test_pins$pin)))), "2014-12-31")  
})
