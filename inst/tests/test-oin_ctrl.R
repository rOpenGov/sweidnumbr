
test_oin <- c("556000-4615", "232100-0156", "802002–4280", "556000-4617", "232100-0152", "802002–4281")
test_oin_res <- rep(FALSE, 6)
test_oin_res[1:3] <- rep(TRUE, 3)

cat("oin_ctrl : ")

test_that(desc="oin_ctrl",{
  expect_equal(oin_ctrl(oin = test_oin), expected = test_oin_res)
})

cat("\n")