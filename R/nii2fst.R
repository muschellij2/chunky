#' Convert Nifti Image into FST Fast file
#'
#' @param file a filename of the image
#' @param fst_file output filename of FST file
#' @param read_in Read in the FST file using
#' [fst::read_fst] and return an FST object rather than a
#' filename
#' @param ... additional arguments to [fst::write_fst]
#'
#' @importFrom RNifti retrieveNifti
#' @importFrom fst write_fst read_fst
#'
#' @return A file path of the FST file or FST data read if
#' `read_in = TRUE`
#' @export
#'
#' @examples
#' if (requireNamespace("oro.nifti", quietly = TRUE) &
#'     requireNamespace("neurobase", quietly = TRUE)) {
#' L = lapply(1:10, function(x) {
#' arr = oro.nifti::nifti(array(rnorm(50^3), dim = rep(50, 3)))
#' tfile = tempfile()
#' neurobase::writenii(arr, tfile)
#' tfile
#' })
#' arr = L[[1]]
#' nii2fst(arr)
#' nii2fst(arr, read_in = TRUE)
#' res = nii2fst_mult(L)
#' res = nii2fst_mult(L, read_in = TRUE)
#' }
nii2fst = function(file, fst_file = NULL, read_in = FALSE, ...) {
  stopifnot(is.character(file))
  img = RNifti::readNifti(file)
  img = c(img)
  img = data.frame(img = img)
  if (is.null(fst_file)) {
    fst_file = tempfile(fileext = ".fst")
  }
  fst::write_fst(img, path = fst_file, ...)
  if (read_in) {
    return(read_fst(fst_file))
  } else {
    return(fst_file)
  }
}


#' @rdname nii2fst
check_image_dims = function(file) {
  file = lapply(file, RNifti::readNifti)
  d = dim(file[[1]])
  check = sapply(file, function(r){
    all(dim(r) == d)
  })
  if (!all(check)) {
    stop("dimensions of the images are not the same!")
  }
  NULL
}


#' @rdname nii2fst
#' @export
nii2fst_mult = function(file, ...) {
  check_image_dims(file)
  img = RNifti::readNifti(file[[1]])
  d = dim(img)
  indices = expand.grid(dim1 = 1:d[1],
                        dim2 = 1:d[2],
                        dim3 = 1:d[3])
  res = lapply(file, nii2fst, ...)
  res = list(indices = indices,
             fst_data = res)
  res
}

