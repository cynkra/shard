test_that("integration", {
  dir <- tempfile("shard")

  shard_write_tsv(iris, "iris", dir, shard_by = c(Species, Sepal.Width))
  iris_out <- shard_read_tsv("iris", dir)

  iris_chr <-
    iris %>%
    mutate_if(is.factor, as.character)

  expect_df_equal(iris_out, iris_chr)
})
