# shard 0.0.0.9002 (2021-02-28)

- Use memoise for caching.
- Clean target directory before writing.
- Faster reading.


# shard 0.0.0.9001 (2021-02-28)

- `shard_read_csv()` and `shard_read_tsv()` gain `col_names` and `col_types` arguments, column specification is read only once for better performance.


# shard 0.0.0.9000 (2020-04-29)

- Initial release, with functions:
    - `shard_read_csv()`
    - `shard_read_tsv()`
    - `shard_write_csv()`
    - `shard_write_tsv()`
    - `shard_split()`
    - `shard_bind()`
