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
#' @return A list of the indices of the image and
#' a file path of the FST file or FST data read if
#' `read_in = TRUE`.
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
#' res = nii2fst(L)
#' res = nii2fst(L, read_in = TRUE)
#' ind = res$indices
#' grab_index = which(ind$dim1 >= 30 & ind$dim2 == 15 & ind$dim3 == 14)
#' mat = sapply(res$data, function(x) x[grab_index,])
#' }
#'
#'
#'
#' # 4d exmample
#' if (requireNamespace("oro.nifti", quietly = TRUE) &
#'     requireNamespace("neurobase", quietly = TRUE)) {
#' L = lapply(1:10, function(x) {
#' arr = oro.nifti::nifti(array(rnorm(10^4), dim = rep(10, 4)))
#' tfile = tempfile()
#' neurobase::writenii(arr, tfile)
#' tfile
#' })
#' arr = L[[1]]
#' nii2fst(arr)
#' nii2fst(arr, read_in = TRUE)
#' res = nii2fst(L)
#' res = nii2fst(L, read_in = TRUE)
#' dim(res$data[[1]])
#' ind = res$indices
#' grab_index = which(ind$dim1 >= 30 & ind$dim2 == 15 & ind$dim3 == 14)
#' mat = sapply(res$data, function(x) x[grab_index,])
#' }

#' @export
nii2fst = function(file, fst_file = NULL, read_in = FALSE, ...) {
  check_image_dims(file)
  img = RNifti::readNifti(file[[1]])
  d = dim(img)
  indices = expand.grid(dim1 = 1:d[1],
                        dim2 = 1:d[2],
                        dim3 = 1:d[3])
  tfile = tempfile(fileext = ".fst")
  write_fst(indices, tfile)
  indices = fst::fst(tfile)
  res = pbapply::pblapply(
    file, .nii2fst,
    fst_file = fst_file,
    read_in = read_in,
    ...)
  if (length(res) == 1 && read_in == FALSE) {
    res = res[[1]]
  }
  res = list(indices = indices,
             data = res)
  class(res) = "niifst_table"
  res
}


#' @rdname nii2fst
check_image_dims = function(file) {
  dims = lapply(file, function(x) {
    max_d = 4
    d = RNifti::niftiHeader(x)$dim
    dd = min(max_d, (d[1] + 2  - 1))
    # d = d[ 2:(d[1] + 2  - 1)]
    d = d[ 2:dd]
  })
  d = dims[[1]]
  check = sapply(dims, function(r){
    all(r == d)
  })
  if (!all(check)) {
    stop("dimensions of the images are not the same!")
  }
  NULL
}


#' @rdname nii2fst
.nii2fst = function(file, fst_file = NULL, read_in = FALSE, ...) {
  stopifnot(is.character(file))
  img = RNifti::readNifti(file)
  nd = dim(img)
  ntim = nd[4]
  if (!is.na(nd[5])) {
    if (nd[5] == 1) {
      img = drop(img)
    }
    nd = dim(img)
    ntim = nd[4]
  }
  if (!is.na(nd[5])) {
    stop("nii2fst really only works up to 4D files")
  }
  if (is.na(ntim)) {
    ntim = 1
  }
  img = c(img)
  img = matrix(img, nrow = prod(nd[1:3]), ncol = ntim)
  img = as.data.frame(img)
  if (ntim == 1) {
    colnames(img) = "img"
  } else {
    colnames(img) = paste0("img_", 1:ntim)
  }
  if (is.null(fst_file)) {
    fst_file = tempfile(fileext = ".fst")
  }
  fst::write_fst(img, path = fst_file, ...)
  if (read_in) {
    return(fst::fst(fst_file))
  } else {
    return(fst_file)
  }
}
