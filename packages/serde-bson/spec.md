## Specification Version 1.1

BSON is a binary format in which zero or more ordered key/value
pairs are stored as a single entity. We call this entity
a _document_.

The following grammar specifies version 1.1 of the
BSON standard. We've written the grammar using a
pseudo- [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form)
syntax. Valid BSON data is represented by
the `document` non-terminal.

### Basic Types

The following basic types are used as terminals in
the rest of the grammar. Each type must be serialized in
little-endian format.

|     |     |
| --- | --- |
| byte | 1 byte (8-bits) |
| signed\_byte(n) | 8-bit, two's complement signed integer for which the value is `n`. |
| unsigned\_byte(n) | 8-bit unsigned integer for which the value is `n`. |
| int32 | 4 bytes (32-bit signed integer, two's complement) |
| int64 | 8 bytes (64-bit signed integer, two's complement) |
| uint64 | 8 bytes (64-bit unsigned integer) |
| double | 8 bytes (64-bit IEEE 754-2008 binary floating point) |
| decimal128 | 16 bytes (128-bit IEEE 754-2008 decimal floating point) |

### Non-terminals

The following specifies the rest of the BSON
grammar. Note that we use the `*` operator as
shorthand for repetition (e.g. `(byte*2)`
is `byte byte`). When used as a unary
operator, `*` means that the repetition can
occur 0 or more times.

|     |     |     |     |
| --- | --- | --- | --- |
| document | ::= | int32 e\_list unsigned\_byte(0) | BSON Document. `int32` is the total number of bytes constituting the document. |
| e\_list | ::= | element e\_list |  |
|  | \| | "" |  |
| element | ::= | signed\_byte(1) e\_name double | 64-bit binary floating point |
|  | \| | signed\_byte(2) e\_name string | UTF-8 string |
|  | \| | signed\_byte(3) e\_name document | Embedded document |
|  | \| | signed\_byte(4) e\_name document | Array. [See below for more information.](https://bsonspec.org/spec.html#more-array) |
|  | \| | signed\_byte(5) e\_name binary | Binary data |
|  | \| | signed\_byte(6) e\_name | Undefined (value). _Deprecated._ |
|  | \| | signed\_byte(7) e\_name (byte\*12) | [ObjectId](https://www.mongodb.com/docs/manual/reference/bson-types/#objectid) |
|  | \| | signed\_byte(8) e\_name unsigned\_byte(0) | Boolean (false) |
|  | \| | signed\_byte(8) e\_name unsigned\_byte(1) | Boolean (true) |
|  | \| | signed\_byte(9) e\_name int64 | UTC datetime. `int64` is UTC milliseconds since the Unix epoch. |
|  | \| | signed\_byte(10) e\_name | Null value |
|  | \| | signed\_byte(11) e\_name cstring cstring | Regular expression. [See below for more information.](https://bsonspec.org/spec.html#more-regex) |
|  | \| | signed\_byte(12) e\_name string (byte\*12) | DBPointer. _Deprecated._ |
|  | \| | signed\_byte(13) e\_name string | JavaScript code |
|  | \| | signed\_byte(14) e\_name string | Symbol. _Deprecated._ |
|  | \| | signed\_byte(15) e\_name code\_w\_s | JavaScript code with scope. _Deprecated._ |
|  | \| | signed\_byte(16) e\_name int32 | 32-bit integer |
|  | \| | signed\_byte(17) e\_name uint64 | Timestamp. A special internal type used by MongoDB replication and sharding. The first 4 bytes are an increment and the second 4 bytes are a timestamp. |
|  | \| | signed\_byte(18) e\_name int64 | 64-bit integer |
|  | \| | signed\_byte(19) e\_name decimal128 | [128-bit\<br> decimal floating point](https://github.com/mongodb/specifications/blob/master/source/bson-decimal128/decimal128.rst) |
|  | \| | signed\_byte(-1) e\_name | Min key. A special type that compares lower than all other possible BSON element values. |
|  | \| | signed\_byte(127) e\_name | Max key. A special type that compares higher than all other possible BSON element values. |
| e\_name | ::= | cstring | Key name |
| string | ::= | int32 (byte\*) unsigned\_byte(0) | String. The `int32` is the number of bytes in the<br> `(byte*)` plus one for the trailing null byte. The `(byte*)` is<br> zero or more UTF-8 encoded characters. |
| cstring | ::= | (byte\*) unsigned\_byte(0) | Zero or more modified UTF-8 encoded characters<br> followed by the null byte. Because the `(byte*)` MUST NOT contain<br> `unsigned_byte(0)`, it is not full UTF-8. |
| binary | ::= | int32 subtype (byte\*) | BSON binary or `BinData`. An array of bytes, similar to a Java `ByteArray`. The `int32` is the number of bytes in the `(byte*)`. The _subtype_ indicates the kind of data in the byte array. |
| subtype | ::= | unsigned\_byte(0) | Generic binary subtype. This is the most commonly used binary subtype and should be the 'default' for drivers and tools. |
|  | \| | unsigned\_byte(1) | Function |
|  | \| | unsigned\_byte(2) | Binary (old). _Deprecated_ in favor of subtype 0. Drivers and tools should properly handle this subtype. [See below for more information.](https://bsonspec.org/spec.html#more-binary-old) |
|  | \| | unsigned\_byte(3) | UUID (old). _Deprecated_ in favor of subtype 4. Drivers and tools for languages with a native UUID type should properly handle subtype 3. |
|  | \| | unsigned\_byte(4) | UUID |
|  | \| | unsigned\_byte(5) | MD5 |
|  | \| | unsigned\_byte(6) | [Encrypted\<br> BSON value](https://github.com/mongodb/specifications/blob/master/source/bson-binary-encrypted/binary-encrypted.md) |
|  | \| | unsigned\_byte(7) | Compressed BSON column. [See below for more information.](https://bsonspec.org/spec.html#more-compressed) |
|  | \| | unsigned\_byte(8) | Sensitive |
|  | \| | unsigned\_byte(9) | Vector. [See below for more information.](https://bsonspec.org/spec.html#more-vector) |
|  | \| | unsigned\_byte(128)—unsigned\_byte(255) | User-defined subtypes |
| code\_w\_s | ::= | int32 string document | Code with scope. _Deprecated._ The `int32` is the length in bytes of the entire `code_w_s` value. The `string` is JavaScript code. The document is a mapping from identifiers to values, representing the scope in which the string should be evaluated. |

### More Information

| Type | Description |
| --- | --- |
| Array | A BSON document with integer values for the keys, starting at 0 and continuing sequentially.<br> For example, the array `['red', 'blue']` encodes as the document `{'0': 'red', '1': 'blue'}`. |
| Regular expression | The first `cstring` is the regex<br>pattern. The second `cstring` is the regex options string.<br>Options are identified by characters, which must be stored in<br>alphabetical order. Regular expressions support the following options:<br>- `i` enables case-insensitive matching<br>- `m` enables multiline matching<br>- `s` enables _dotall_ mode ("." matches everything)<br>- `x` enables verbose mode<br>- `u` enables "\\w", "\\W", etc. to match Unicode |
| Binary (old) | The structure of the binary data must be an `int32` followed by a `byte*`. The `int32` is the number of bytes in the repetition. |
| Compressed BSON column | This data type uses delta and delta-of-delta compression and run-length-encoding for efficient element storage. It also has an encoding for sparse arrays containing missing values. |
| Vector | Dense array of numeric values stored in a binary format efficient for storage and retrieval. Vectors are effectively used to represent data in artificial intelligence, machine learning, semantic search, computer vision, and natural language processing applications.<br>All values within the vector must be of the same type. Vectors support the following types:<br>- Packed binary (1-bit unsigned `int`)<br>- Signed 8-bit integer (`int`)<br>- 32-bit floating point (`float`)<br>For more information about BSON vectors, see the [BSON Binary Subtype 9](https://github.com/mongodb/specifications/blob/master/source/bson-binary-vector/bson-binary-vector.md) specification document. |
