# big_a -------------------------------------------------------------------
test_that("big_a calculation is done correctly for a few values", {

  quart <- angularsweep:::big_a(1,2,3,4)
  half <- angularsweep:::big_a(1,1,1,2)
  three_quart <- angularsweep:::big_a(-1,2,-3,4)
  one <- angularsweep:::big_a(-1,0,-2,0)
  mquart <- angularsweep:::big_a(1,-2,3,-4)
  mhalf <- angularsweep:::big_a(1,-1,1,-2)
  mthree_quart <- angularsweep:::big_a(-1,-2,-3,-4)

  expect_equal(quart, pi/4)
  expect_equal(half, pi/2)
  expect_equal(three_quart, 3*pi/4)
  expect_equal(one, pi)
  expect_equal(mquart, -pi/4)
  expect_equal(mhalf, -pi/2)
  expect_equal(mthree_quart, -3*pi/4)
})

test_that("big_a error testing", {

  testthat::expect_error(angularsweep:::big_a(1,1,1,1), regexp = "distinct")
})

# big_b -------------------------------------------------------------------
test_that("big_b calculation is done correctly for a few values", {

  val_1 <- angularsweep:::big_b(2,4)
  val_2 <- angularsweep:::big_b(4,4)
  val_3 <- angularsweep:::big_b(Inf,4)

  expect_equal(val_1, 0)
  expect_equal(val_2, pi/3)
  expect_equal(val_3, pi/2)
})

test_that("big_b error testing", {

  testthat::expect_error(angularsweep:::big_b(10, 0), regexp = "distinct")
  testthat::expect_error(angularsweep:::big_b(10, -5), regexp = "less than zero")
  testthat::expect_error(angularsweep:::big_b(4, 10), regexp = "half")
})


# normalise_angle ---------------------------------------------------------
test_that("normalise_angle calculation is done correctly for a few values", {

  thetas <- seq(-4*pi, 4*pi, by = pi/2)

  normed <- angularsweep:::normalise_angle(thetas)

  expect_lte(max(normed), pi)
  expect_lte(min(normed), -pi)
  expect_equal(normed, c(0, 1.5707963267949, -3.14159265358979, -1.5707963267949, 0,
                         1.5707963267949, -3.14159265358979, -1.5707963267949, 0, 1.5707963267949,
                         3.14159265358979, -1.5707963267949, 0, 1.5707963267949, -3.14159265358979,
                         -1.5707963267949, 0))
})


# pol2cart ----------------------------------------------------------------
test_that("pol2cart calculation is done correctly for a few values", {

  r <- 1:5
  theta <- seq(-pi, pi, length.out = 5)

  normed <- angularsweep:::pol2cart(r, theta)

  expect_equal(normed, structure(c(-1, 1.22464679914735e-16, 3, 2.44929359829471e-16,
                                   -5, -1.22464679914735e-16, -2, 0, 4, 6.12323399573677e-16),
                                 .Dim = c(5L,
                                          2L), .Dimnames = list(NULL, c("x", "y"))))
})

test_that("pol2cart calculates correctly using origin", {

  r <- 11:15
  origin_1 <- c(x = 10, y = 10)
  origin_2 <- c(x = 20, y = 20)
  theta <- seq(-pi, pi, length.out = 5)

  normed_1 <- angularsweep:::pol2cart(r, theta, origin = origin_1)
  normed_2 <- angularsweep:::pol2cart(r, theta, origin = origin_2)

  expect_equal(normed_1 + 10, normed_2)
})


