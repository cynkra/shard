verify_output("spec.txt", {
  iris %>%
    build_shard_spec("iris", "csv")

  iris %>%
    build_shard_spec("iris", "csv", shard_by = Species)

  iris %>%
    build_shard_spec("iris", "csv", shard_by = c(Species, Petal.Width))

  iris %>%
    slice(0) %>%
    build_shard_spec("iris", "csv", shard_by = c(Species, Petal.Width))
})
