testthat::context("Testing fst works on 3D")


testthat::test_that("Checking 3D example", {
  if (requireNamespace("oro.nifti", quietly = TRUE) &
      requireNamespace("neurobase", quietly = TRUE)) {

    nd = rep(20, 3)
    L = lapply(1:10, function(x) {
      arr = oro.nifti::nifti(array(rnorm(prod(nd)),
                                   dim = nd))
      tfile = tempfile()
      neurobase::writenii(arr, tfile)
      tfile
    })
    arr = L[[1]]
    out = nii2fst(arr)
    testthat::expect_is(out$data, "character")
    testthat::expect_is(out$indices, "fst_table")
    testthat::expect_is(out, "niifst_table")
    testthat::expect_true(file.exists(out$data))
    read_in = nii2fst(arr, read_in = TRUE)
    testthat::expect_is(read_in$data, "list")
    testthat::expect_equal(ncol(read_in$data[[1]]), 1)
    testthat::expect_is(read_in, "niifst_table")

    res = nii2fst(L, read_in = TRUE)
    testthat::expect_is(res, "niifst_table")
    testthat::expect_equal(nrow(res$indices), prod(nd[1:3]))


    checks = sapply(res$data, function(x) {
      testthat::expect_equal(nrow(res$indices), nrow(x))
      invisible(NULL)
    })

  }
})
