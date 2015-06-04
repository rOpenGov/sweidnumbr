context("S3 methods")

x <- as.pin(c(191212121212, 198505043334))

test_that("class attribut preserved", {
  expect_is(x[1], "pin")
  expect_is({x[1] <- "8505043334"; x}, "pin")
  expect_true(nchar(x[1])==12)
  expect_warning(x[1] <- "hejsan svejsan")
  expect_true(is.na(x[1]))
  })
