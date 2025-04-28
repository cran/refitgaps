test_that("the number of holes is checked", {
  ORR <- recast(MOZ, TPL, Niter = 100)
  HG <- have_gaps(ORR[[1]])
  wg <- paste0(stringr::str_extract(HG$ore, "\\*.+\\*"), collapse="")
  expect_equal(ORR[[2]], stringr::str_count(wg, "-"))
})

