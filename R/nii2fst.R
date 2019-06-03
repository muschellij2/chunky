#' Convert Nifti Image into FST Fast file
#'
#' @param x an object that [RNifti::retrieveNifti]
#' @param fst_file output filename of FST file
#' @importFrom RNifti retrieveNifti
#' @importFrom fst write_fst read_fst
#'
#' @return A file path
#' @export
#'
#' @examples
#' arr = array(rnorm(50^3), dim = rep(50, 3))
nii2fst = function(x, fst_file = NULL) {
  img = RNifti::retrieveNifti(object = x)
  img = c(img)
  img = data.frame(img = img)
  if (is.null(fst_file)) {
    fst_file = tempfile(fileext = ".fst")
  }
  fst::write_fst(img, path = fst_file)
  fst_file
}

