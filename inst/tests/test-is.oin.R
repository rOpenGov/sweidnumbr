
test_oin <- c("556000-4615", "232100-0156", "802002–4280", "8020024280", "802002A4280", "8020A2-4280","801002-4280", "801002-428 ")
test_oin_res <- rep(FALSE, 8)
test_oin_res[1:3] <- TRUE

cat("is.oin : ")

test_that(desc="is.pin",{
  expect_equal(is.oin(oin = test_oin), expected = test_oin_res)
})

cat("\n")