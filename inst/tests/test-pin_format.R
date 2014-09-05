
today_pin <- substr(paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),"0000",sep=""), 3, 12)
tomorrow_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()+1),split = "-")), collapse = ""),"0000",sep="")

cat("pin_format : ")

test_that(desc="numeric: YYYYMMDDNNNC",{
  expect_equal(pin_format(196408233234), expected = "196408233234")
  expect_equal(pin_format(200108230000), expected = "200108230000")  
  expect_is(pin_format(200108230000), "character")  
})

test_that(desc="numeric: YYMMDDNNNC",{
  expect_equal(pin_format(6408233234), expected = "196408233234")  
  expect_equal(pin_format(108230000), expected = "200108230000")  
  expect_equal(pin_format(8230000), expected = "200008230000")  
})

test_that(desc="character: 'YYMMDDNNNC'",{
  expect_equal(pin_format("6408233234"), expected = "196408233234")  
  expect_equal(pin_format("0008230000"), expected = "200008230000")
  expect_equal(pin_format(today_pin), expected = paste("20",today_pin, sep=""))  
})

test_that(desc="character: 'YYYYMMDDNNNC'",{
  expect_equal(pin_format("196408233234"), expected = "196408233234")  
})

test_that(desc="different formats",{
  
  #' pin_format(ex_pin1)
  #' ex_pin2 <- c()
  expect_equal(pin_format(c("196408233234", "640823-3234", "19640823-3234", "6408233234")), 
                          expected = rep("196408233234", 4))
  expect_equal(pin_format(c(196408233234, 6408233234)), 
               expected = rep("196408233234", 2))
})

test_that(desc="character: 'YYMMDD-NNNC'",{
  expect_equal(pin_format("640823-3234"), expected = "196408233234")  
  expect_equal(pin_format("000823-0000"), expected = "200008230000")
  expect_equal(pin_format("000823+0000"), expected = "190008230000")
})

test_that(desc="error expected",{
  suppressWarnings(expect_equal(pin_format(tomorrow_pin), as.character(NA)))
  suppressWarnings(expect_equal(pin_format(pin = "AA6408233234"), as.character(NA)))
  suppressWarnings(expect_equal(pin_format("196418233234"), as.character(NA)))
  suppressWarnings(expect_equal(pin_format("196408333234"), as.character(NA)))
  expect_warning(pin_format(tomorrow_pin))
  expect_warning(pin_format("AA6408233234"))
  expect_warning(pin_format("196418233234"))
  expect_warning(pin_format("196408333234"))
})

cat("\n")