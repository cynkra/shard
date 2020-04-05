expect_df_equal <- function(object, expected) {
  expect_equal(sort(names(object)), sort(names(expected)))

  expect_equal(nrow(anti_join(object, expected, by = names(object))), 0)
  expect_equal(nrow(anti_join(expected, object, by = names(object))), 0)
}
