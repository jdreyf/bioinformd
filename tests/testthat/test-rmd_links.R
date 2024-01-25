test_that("links work", {
  rl <- rmd_links(filenames = "heat.pdf")
  expect_equal(rl, "`r make_file_links(wd, 'heat.pdf')`")
})
