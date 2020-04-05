verify_output("split.txt", {
  iris %>%
    shard_split("iris", "csv")

  iris %>%
    shard_split("iris", "csv", shard_by = Species)

  iris %>%
    shard_split("iris", "csv", shard_by = c(Species, Petal.Width))

  iris %>%
    slice(0) %>%
    shard_split("iris", "csv", shard_by = c(Species, Petal.Width))
})
