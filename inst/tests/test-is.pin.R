
tomorrow_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()+1),split = "-")), collapse = ""),"0000",sep="")
test_pin <- c("196408233234", tomorrow_pin, "AA6408323234", "19640823323", "1964083332349", "196408333234", "19640823-334")
test_pin_res <- c(TRUE, rep(FALSE, 6))

cat("is.pin : ")


test_that(desc="is.pin",{
  expect_is(is.pin(pin = test_pin), "logical")
  expect_equal(is.pin(pin = test_pin), expected = test_pin_res)
})

cat("\n")