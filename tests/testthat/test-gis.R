test_that("lonlat_to_utm testing", {
  expect_equal(angularsweep:::lonlat_to_utm(), 32631)
  expect_equal(angularsweep:::lonlat_to_utm(c(lon = -74, lat = 41)), 32618)
})
