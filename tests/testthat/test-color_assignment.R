context("ps color assignment tests")
library(here)
library(phyloseq)
library(dplyr)

data(GlobalPatterns)
mdf <- prep_mdf(GlobalPatterns)

test_that("prep_mdf melts", expect_s3_class(prep_mdf(GlobalPatterns), "data.frame"))

test_that("default hex group range", expect_error(default_hex(7)))

test_that("default hex return correct # groups", expect_length(default_hex(3), 3))

test_that("create c/mdf", expect_type(create_color_dfs(mdf), "list"))

test_that("mdf new cols created", expect_length(create_color_dfs(mdf)$mdf, length(mdf) + 3))

test_that("match_cdf identify invalid input", expect_error(match_cdf(mdf, mdf)))

test_that("reorder_samples_by invalid input", expect_error(reorder_samples_by(mdf, mdf)))

test_that("plot_microshades invalid objects", expect_error(plot_microshades(mdf, mdf)))
