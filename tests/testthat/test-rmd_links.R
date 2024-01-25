test_that("multiplication works", {
  rl <- rmd_links(filenames = "heat.pdf")
  expect_equal(rl, "`r make_file_links(wd, 'heat.pdf')`")
})
