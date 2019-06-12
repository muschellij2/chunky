testthat::context("Testing errors")


testthat::test_that("Checking 5D example", {
  if (requireNamespace("oro.nifti", quietly = TRUE) &
      requireNamespace("neurobase", quietly = TRUE)) {

    nd = rep(10, 5)
    L = lapply(1:3, function(x) {
      arr = oro.nifti::nifti(array(rnorm(prod(nd)),
                                   dim = nd))
      tfile = tempfile()
      neurobase::writenii(arr, tfile)
      tfile
    })
    arr = L[[1]]
    testthat::expect_error(nii2fst(arr),
                           regexp = "only works")
    testthat::expect_error(nii2fst(L),
                           regexp = "only works")

  }
})

testthat::test_that("Checking 5D with drop example", {
  if (requireNamespace("oro.nifti", quietly = TRUE) &
      requireNamespace("neurobase", quietly = TRUE)) {
    nd = c(rep(10, 4), 1)
    L = lapply(1:3, function(x) {
      arr = oro.nifti::nifti(array(rnorm(prod(nd)),
                                   dim = nd))
      tfile = tempfile()
      neurobase::writenii(arr, tfile)
      tfile
    })
    arr = L[[1]]
    testthat::expect_silent({out = nii2fst(arr)})
    testthat::expect_is(out$data, "character")
    testthat::expect_is(out$indices, "fst_table")
    testthat::expect_is(out, "niifst_table")
    testthat::expect_true(file.exists(out$data))
  }
})


testthat::test_that("Not same dimensions", {
  if (requireNamespace("oro.nifti", quietly = TRUE) &
      requireNamespace("neurobase", quietly = TRUE)) {
    nd = rep(10, 3)
    L = lapply(1:2, function(x) {
      arr = oro.nifti::nifti(array(rnorm(prod(nd)),
                                   dim = nd))
      tfile = tempfile()
      neurobase::writenii(arr, tfile)
      tfile
    })
    img = neurobase::readnii(L[[2]])
    arr = img[,,1:9]
    img = neurobase::copyNIfTIHeader(img = img, arr = arr)
    tfile = tempfile()
    neurobase::writenii(img, tfile)
    L[[2]] = tfile
    testthat::expect_error({out = nii2fst(L)},
                           regexp = "dimensions.*same")
  }
})
