# mysql

MySQL/InnoDB driver for `sqlx`.

The package implements `Sqlx_driver.Driver.Intf` and is intended for MySQL 8.x
and compatible servers that speak the MySQL 4.1+ protocol.

```ocaml
open Std
open Sqlx

module Db = Sqlx.Make (Mysql.Driver)

let config =
  Mysql.Config.{
    (default ()) with
    database = Some "app";
    user = "app";
    password = "secret";
  }
```

Parameterized SQL uses `?` placeholders:

```ocaml
let user =
  Db.query_one pool "SELECT id, email FROM users WHERE id = ?" [ Value.int64 id ]
```

Supported authentication methods:
- `mysql_native_password`
- `caching_sha2_password` fast authentication
- `caching_sha2_password` full authentication over TLS

Full `caching_sha2_password` RSA key exchange on plaintext connections is
rejected with a structured driver error. Use `ssl_mode = Require` for servers
that need full authentication.

For migrations, use `Sqlx.Migrate.Config.for_mysql ()`. It selects MySQL
placeholders, creates the migration table with `ENGINE=InnoDB`, uses
`GET_LOCK`/`RELEASE_LOCK`, and runs migration bodies outside an explicit
transaction by default because MySQL DDL usually commits implicitly.
