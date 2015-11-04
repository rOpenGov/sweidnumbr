
context("as.oin")

test_that(desc="class: oin",{
  expect_is(as.oin("556000-4615"), class = "oin")
})

test_that(desc="character: GNNNNNN-NNNC",{
  expect_equal(as.character(as.oin(c("556000-4615", "232100-0156", "802002-4280"))), 
               expected = c("556000-4615", "232100-0156", "802002-4280"))
})

test_that(desc="error expected",{
  expect_equal(as.character(suppressWarnings(as.oin(c("8020024280", "AA2002-4280")))), as.character(c(NA,NA)))  
})





test_oins <- c("556000-4615", "232100-0156", "802002-4280")
test_that("Recycling rules", {
  expect_is(data.frame(as.oin(test_oins), 1:9), "data.frame")
  expect_equal(nrow(data.frame(as.oin(test_oins), 1:9)), 9)
  expect_equal(data.frame(as.oin(test_oins), 1:9)[1:3, 1], data.frame(as.oin(test_oins), 1:9)[4:6, 1])
  expect_equal(data.frame(as.oin(test_oins), 1:9)[1:3, 1], data.frame(as.oin(test_oins), 1:9)[7:9, 1])
})

