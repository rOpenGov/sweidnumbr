
today_pin <- substr(paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),"0000",sep=""), 3, 12)
tomorrow_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()+1),split = "-")), collapse = ""),"0000",sep="")

context("as.pin")

test_that(desc="class: pin",{
  expect_is(as.pin("196408233234"), class = "pin")
})

test_that(desc="numeric: YYYYMMDDNNNC",{
  expect_equal(as.character(suppressMessages(as.pin(196408233234))), expected = "196408233234")
  expect_equal(as.character(suppressMessages(as.pin(200108230000))), expected = "200108230000")  
  expect_is(as.character(suppressMessages(as.pin(200108230000))), "character")  
  skip_on_cran()
  skip_on_travis()
  expect_is(as.character(suppressMessages(as.pin(c(NA,198501169885)))), "character")
})

test_that(desc="numeric: YYMMDDNNNC",{
  expect_equal(as.character(suppressMessages(as.pin(6408233234))), expected = "196408233234")
  expect_equal(as.character(suppressMessages(as.pin(108230000))), expected = "200108230000")
  expect_equal(as.character(suppressMessages(as.pin(pin = c(8230000,108230000)))), expected = c("200008230000", "200108230000"))
  skip_on_cran()
  skip_on_travis()
  expect_is(as.character(suppressMessages(as.pin(c(NA,8501169885)))), "character")
})

test_that(desc="character: 'YYMMDDNNNC'",{
  expect_equal(as.character(suppressMessages(as.pin("6408233234"))), expected = "196408233234")  
  expect_equal(as.character(suppressMessages(as.pin("0008230000"))), expected = "200008230000")
  expect_equal(as.character(suppressMessages(as.pin(today_pin))), expected = paste("20",today_pin, sep=""))  
  expect_is(as.character(suppressMessages(as.pin(c(NA,"8501169885")))), "character")
  skip_on_cran()
  skip_on_travis()
  expect_false(is.na(as.character(suppressMessages(as.pin("640228P367")))))
})


test_that(desc="character: 'YYYYMMDDNNNC'",{
  expect_equal(as.character(suppressMessages(as.pin("196408233234"))), expected = "196408233234")  
  expect_is(as.character(suppressMessages(as.pin(c(NA,"198501169885")))), "character")
  skip_on_cran()
  skip_on_travis()
  expect_false(is.na(as.character(suppressMessages(as.pin("19640228P367")))))
})

test_that(desc="different formats",{
  expect_equal(as.character(suppressMessages(as.pin(c("196408233234", "640823-3234", "19640823-3234", "6408233234")))), 
                          expected = rep("196408233234", 4))
  expect_equal(as.character(suppressMessages(as.pin(c(196408233234, 6408233234)))), 
               expected = rep("196408233234", 2))
})

test_that(desc="character: 'YYMMDD-NNNC'",{
  expect_equal(as.character(as.pin("640823-3234")), expected = "196408233234")  
  expect_equal(as.character(as.pin("000823-0000")), expected = "200008230000")
  expect_equal(as.character(as.pin("000823+0000")), expected = "190008230000")
  skip_on_cran()
  skip_on_travis()
  expect_false(is.na(as.character(suppressMessages(as.pin("640228-P367")))))
})

test_that(desc="error expected",{
  suppressWarnings(expect_equal(as.character(as.pin(tomorrow_pin)), as.character(NA)))
  suppressWarnings(expect_equal(as.character(as.pin(pin = "AA6408233234")), as.character(NA)))
  suppressWarnings(expect_equal(as.character(as.pin("196418233234")), as.character(NA)))
  suppressWarnings(expect_equal(as.character(as.pin("196408333234")), as.character(NA)))
  expect_warning(as.pin(tomorrow_pin))
  expect_warning(as.pin("AA6408233234"))
  expect_warning(as.pin("196418233234"))
  expect_warning(as.pin("196408333234"))
  
  test_pin <- c("196408233234", tomorrow_pin, "AA6408323234", "19640823323", "1964083332349", "196408333234", "19640823-334")
  test_pin_res <- c(TRUE, rep(FALSE, 6))
  suppressWarnings(expect_equal(!is.na(as.pin(test_pin)), test_pin_res))
  
})
