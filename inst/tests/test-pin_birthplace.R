
cat("pin_birthplace : ")

today_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),"0000",sep="")
pin_test <- c("198111219876", "198111819876","196408233234", "196408833234", today_pin, "196408830000")
pin_test_res <-
  c(rep("Extra number and immigrants (immigrated after 1946)", 2),
    rep("Gotlands län", 2),
    "Born after 31 december 1989",
    "Stockholm stad")

test_that(desc="birthplace",{
  expect_equal(pin_birthplace(pin = pin_test), expected = as.factor(pin_test_res))
})

cat("\n")