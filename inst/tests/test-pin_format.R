
today_pin <- substr(paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),"0000",sep=""), 3, 12)
tomorrow_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()+1),split = "-")), collapse = ""),"0000",sep="")

cat("pin_format : ")

test_that(desc="numeric: YYYYMMDDNNNC",{
  expect_equal(pin_format(pin = 198112189876), expected = "198112189876")
  expect_equal(pin_format(196408233234), expected = "196408233234")
  expect_equal(pin_format(200108230000), expected = "200108230000")  
  expect_is(pin_format(200108230000), "character")  
})

test_that(desc="numeric: YYMMDDNNNC",{
  expect_equal(pin_format(8112189876), expected = "198112189876")
  expect_equal(pin_format(6408233234), expected = "196408233234")  
})

test_that(desc="character: 'YYMMDDNNNC'",{
  expect_equal(pin_format("8112189876"), expected = "198112189876")
  expect_equal(pin_format("6408233234"), expected = "196408233234")  
  expect_equal(pin_format("0008230000"), expected = "200008230000")
  expect_equal(pin_format(today_pin), expected = paste("20",today_pin, sep=""))  
})

test_that(desc="character: 'YYYYMMDDNNNC'",{
  expect_equal(pin_format("198112189876"), expected = "198112189876")
  expect_equal(pin_format("196408233234"), expected = "196408233234")  
})

test_that(desc="different formats",{
  expect_equal(pin_format(c("198112189876", "8112189876", "811218-9876")), 
                          expected = rep("198112189876",3))
  expect_equal(pin_format(c(198112189876, 8112189876)), 
               expected = rep("198112189876", 2))
})

test_that(desc="character: 'YYMMDD-NNNC'",{
  expect_equal(pin_format("811218-9876"), expected = "198112189876")
  expect_equal(pin_format("640823-3234"), expected = "196408233234")  
  expect_equal(pin_format("000823-0000"), expected = "200008230000")
  expect_equal(pin_format("000823+0000"), expected = "190008230000")
})

test_that(desc="error expected",{
  suppressWarnings(expect_equal(pin_format(tomorrow_pin), as.character(NA)))
  suppressWarnings(expect_equal(pin_format(pin = "AA8112189876"), as.character(NA)))
  suppressWarnings(expect_equal(pin_format("198122189876"), as.character(NA)))
  suppressWarnings(expect_equal(pin_format("198112489876"), as.character(NA)))
  expect_warning(pin_format(tomorrow_pin))
  expect_warning(pin_format("AA8112189876"))
  expect_warning(pin_format("198122189876"))
  expect_warning(pin_format("198112489876"))
})

cat("\n")