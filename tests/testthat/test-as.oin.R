
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
