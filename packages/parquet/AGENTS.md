# parquet AGENTS

`parquet` owns Riot's standalone Parquet file reader/writer.

## Rules

1. Keep the package centered on Parquet's own file model: header/body/footer, compact-thrift metadata, and page/column structures.
2. Maintain wire compatibility for the Parquet magic bytes, footer tail, and compact-thrift metadata encoding.
3. Prefer `IO.Reader` and `IO.Writer` based APIs for file I/O. If `from_reader` needs full-file buffering because the footer is at EOF, document that behavior.
4. Unknown thrift fields in metadata should be skipped safely so the reader remains forward-compatible.
5. Keep typed row decoding in Parquet's own reader layer.
