verify_output("spec.txt", {
  iris %>%
    build_shard_spec("iris", "csv") %>%
    expand_first_nested()

  iris %>%
    build_shard_spec("iris", "csv", shard_by = Species) %>%
    expand_first_nested()

  iris %>%
    build_shard_spec("iris", "csv", shard_by = c(Species, Petal.Width)) %>%
    expand_first_nested()

  iris %>%
    slice(0) %>%
    build_shard_spec("iris", "csv", shard_by = c(Species, Petal.Width)) %>%
    expand_first_nested()
})
