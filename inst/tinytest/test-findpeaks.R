x <- c(2, 12, 4, 6, 9, 4, 3, 1, 19, 7)
expect_identical(findpeaks(x), matrix(c(12,9,19, 2,5,9, 1,3,8, 3,8,10), nrow=3, ncol=4))
expect_identical(findpeaks(x, npeaks = 1, sortstr = TRUE), matrix(c(19, 9, 8, 10), nrow = 1))
expect_identical(findpeaks(x, minpeakheight = 15), matrix(c(19, 9, 8, 10), nrow = 1))
expect_identical(findpeaks(x, threshold = 10), matrix(c(19, 9, 8, 10), nrow = 1))

if (at_home() && requireNamespace("pracma", quietly = TRUE)) {
  expect_equal(findpeaks(x), pracma::findpeaks(x))
  expect_equal(findpeaks(x, npeaks = 1, sortstr = TRUE), pracma::findpeaks(x, npeaks = 1, sortstr = TRUE))
  expect_equal(findpeaks(x, minpeakheight = 15), pracma::findpeaks(x, minpeakheight = 15))
  expect_equal(findpeaks(x, threshold = 10), pracma::findpeaks(x, threshold = 10))
  expect_equal(findpeaks(pracma2:::peaks, nups = 3), pracma::findpeaks(pracma2:::peaks, nups = 3))
}
