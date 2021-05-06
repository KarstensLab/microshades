context("palette tests")

# test palette return correct number of shades
test_that("palette size",
          expect_equal(3, length(microshades_palette("micro_green", 3))))

# test that palette does not exceed palette size limits
test_that("limited palette size",
          expect_error(7, length(microshades_palette("micro_green", 7))))
