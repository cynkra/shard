test_that("bind is the opposite of split", {
  iris_chr <-
    iris %>%
    mutate_if(is.factor, as.character)

  iris_rebound <-
    iris %>%
    shard_split("iris", "csv") %>%
    shard_bind()
  expect_df_equal(iris_rebound, iris)

  iris_rebound <-
    iris %>%
    shard_split("iris", "csv", shard_by = Species) %>%
    shard_bind()
  expect_df_equal(iris_rebound, iris_chr)

  iris_rebound <-
    iris %>%
    shard_split("iris", "csv", shard_by = c(Species, Petal.Width)) %>%
    shard_bind()
  expect_df_equal(iris_rebound, iris_chr)

  iris_rebound <-
    iris %>%
    slice(0) %>%
    shard_split("iris", "csv") %>%
    shard_bind()
  expect_df_equal(iris_rebound, iris_chr[0, ])
})
