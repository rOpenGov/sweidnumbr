context("S3 methods")

x <- as.pin(c(191212121212, 198505043334))

test_that("class attribut preserved", {
  expect_is(x[1], "pin")
  expect_is({x[1] <- "8505043334"; x}, "pin")
  expect_error(x[1] <- "hejsan svejsan")
  })
