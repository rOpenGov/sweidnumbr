
tomorrow_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()+1),split = "-")), collapse = ""),"0000",sep="")
test_pin <- c("198112189876", tomorrow_pin, "AA8112189876", "19811218987", "198122189876", "198112489876", "19811248-876")
test_pin_res <- c(TRUE, rep(FALSE, 6))

cat("is.pin : ")


test_that(desc="is.pin",{
  expect_equal(is.pin(pin = test_pin), expected = test_pin_res)
})

cat("\n")