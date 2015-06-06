
context("pin_birthplace")

today_pin <- paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),"0000",sep="")
pin_test <- c("0000000019876", "187001019876","196408233234", "196408833234", today_pin, "196408830000")
pin_test_res <-
  c(NA, "Extra number and immigrants (immigrated after 1946)",
    rep("Gotlands l\u00E4n", 2),
    "Born after 31 december 1989",
    "Stockholm stad")

test_that(desc="birthplace",{
  suppressWarnings(expect_equal(pin_birthplace(pin = pin_test), expected = as.factor(pin_test_res)))
  suppressWarnings(expect_is(pin_birthplace(pin = pin_test), "factor"))
})

test_that(desc="Handle NA, interim and coordn in pin_birthplace",{
  skip_on_cran()
  skip_on_travis()
  suppressWarnings(expect_true(is.na(pin_birthplace(pin = as.pin(c("hejbaberiba","198501169885")))[1])))
  expect_true(all(is.na(pin_birthplace(pin = c("19000625P816","190006859816")))))
})
