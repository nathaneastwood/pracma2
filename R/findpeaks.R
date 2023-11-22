#' Find Peaks
#'
#' Find peaks (maxima) in a time series.
#'
#' This is a reimplementation of `pracma::findpeaks()` which uses a character string and a regex to identify the peaks.
#' This method is very slow and so this reimplementation using numeric values and the [rle()] instead.
#'
#' @param x `numeric(n)`. Numerical vector taken as a time series (no `NA`s allowed).
#' @param nups `integer(1)`. Minimum number of increasing steps before a peak is reached.
#' @param ndowns `integer(1)`. Minimum number of decreasing steps after the peak.
#' @param zero `integer(1)`. Can be `1`, `-1`, or `0` (the default); how to interpret succeeding steps of the same
#' value: increasing, decreasing, or special.
#' @param minpeakheight `integer(1)`. The minimum (absolute) height a peak has to have to be recognized as such.
#' @param minpeakdistance `integer(1)`. The minimum distance (in indices) peaks have to have to be counted.
#' @param threshold `integer(1)`. The minimum peak size.
#' @param npeaks `integer(1)`. The number of peaks to return.
#' @param sortstr `logical(1)`. Should the peaks be returned sorted in decreasing order of their maximum value?
#'
#' @export
findpeaks <- function(
  x,
  nups = 1,
  ndowns = nups,
  zero = 0,
  minpeakheight = -Inf,
  minpeakdistance = 1,
  threshold = 0,
  npeaks = 0,
  sortstr = FALSE
) {
  stopifnot(is.vector(x, mode = "numeric") || length(is.na(x)) == 0)
  stopifnot(is.numeric(nups), length(nups) == 1L)
  stopifnot(is.numeric(ndowns), length(ndowns) == 1L)
  if (!zero %in% c(0, 1, -1)) stop("Argument 'zero' can only be 0, 1, or -1.")
  stopifnot(is.numeric(minpeakheight), length(minpeakheight) == 1L)
  stopifnot(is.numeric(minpeakdistance), length(minpeakdistance) == 1L)
  stopifnot(is.numeric(threshold), length(threshold) == 1L)
  stopifnot(is.numeric(npeaks), length(npeaks) == 1L)
  stopifnot(is.logical(sortstr), length(sortstr) == 1L, !is.na(sortstr))

  xc <- sign(diff(x))
  # transform '0' to zero
  if (zero != 0) xc[xc == 0] <- zero

  # identify peaks and get indices
  rc <- rle(xc)
  vals <- rc$values
  lens <- rc$lengths
  rc_len <- length(rc$lengths)
  lead <- c(vals[2:rc_len], NA)
  lead_lens <- c(lens[2:rc_len], NA)
  pos_peak_start <- which(vals == 1 & lens >= nups & lead == -1 & lead_lens >= ndowns)
  x1 <- (cumsum(c(0, lens)) + 1)[pos_peak_start]
  lag <- c(NA, vals[-rc_len])
  lag_lens <- c(NA, lens[-rc_len])
  pos_peak_end <- which(vals == -1 & lens >= ndowns & lag == 1 & lag_lens >= nups)
  x2 <- (cumsum(lens) + 1)[pos_peak_end]

  # find index positions and maximum values
  n <- length(x1)
  xv <- xp <- numeric(n)
  for (i in 1:n) {
    xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
    xv[i] <- x[xp[i]]
  }

  # eliminate peaks that are too low
  inds <- which(xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >= threshold)

  # combine into a matrix format
  X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])

  # eliminate peaks that are near by
  if (minpeakdistance < 1)
    warning("Handling 'minpeakdistance < 1' is logically not possible.")

  # sort according to peak height
  if (sortstr || minpeakdistance > 1) {
    sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
    X <- X[sl, , drop = FALSE]
  }

  # return NULL if no peaks
  if (length(X) == 0) return(c())

  # find peaks sufficiently distant
  if (minpeakdistance > 1) {
    no_peaks <- nrow(X)
    badpeaks <- rep(FALSE, no_peaks)

    # eliminate peaks that are close to bigger peaks
    for (i in seq_len(no_peaks)) {
      ipos <- X[i, 2]
      if (!badpeaks[i]) {
        dpos <- abs(ipos - X[, 2])
        badpeaks <- badpeaks | (dpos > 0 & dpos < minpeakdistance)
      }
    }
    # select the good peaks
    X <- X[!badpeaks, , drop = FALSE]
  }

  # Return only the first 'npeaks' peaks
  if (npeaks > 0 && npeaks < nrow(X)) {
    X <- X[1:npeaks, , drop = FALSE]
  }

  return(X)
}
