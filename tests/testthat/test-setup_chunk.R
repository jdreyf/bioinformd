test_that("multiplication works", {
  sc <- setup_chunk("patti")
  expect_equal(sc[length(sc) - 1], "wd <- J:/cores/bioinformatics/patti")
})
