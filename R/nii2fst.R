#' Convert Nifti Image into FST Fast file
#'
#' @param x an object that [RNifti::retrieveNifti]
#' @param fst_file output filename of FST file
#' @param ... additional arguments to [fst::write_fst]
#' @importFrom RNifti retrieveNifti
#' @importFrom fst write_fst read_fst
#'
#' @return A file path of the FST file
#' @export
#'
#' @examples
#' arr = array(rnorm(50^3), dim = rep(50, 3))
#' nii2fst(arr)
nii2fst = function(x, fst_file = NULL, ...) {
  img = RNifti::retrieveNifti(object = x)
  img = c(img)
  img = data.frame(img = img)
  if (is.null(fst_file)) {
    fst_file = tempfile(fileext = ".fst")
  }
  fst::write_fst(img, path = fst_file, ...)
  fst_file
}


#' @rdname nii2fst
check_image_dims = function(x) {
  x = lapply(x, RNifti::retrieveNifti)
  d = dim(x[[1]])
  check = sapply(x, function(r){
    all(dim(r) == d)
  })
  if (!all(check)) {
    stop("dimensions of the images are not the same!")
  }
  NULL
}


#' @rdname nii2fst
#' @export
nii2fst_par = function(x) {
  check_image_dims(x)
  res = lapply(x, nii2fst)
  res
}

