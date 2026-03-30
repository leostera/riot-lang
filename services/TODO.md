# Services TODO

## Registry metadata migration

- [done] Move registry control-plane state from R2 JSON records into D1.
- [done] Add D1 tables for users, oauth states, sessions, API tokens, package claims, published releases, selector resolutions, and lightweight web views.
- [done] Keep only heavy immutable artifacts in R2:
  - source tarballs
  - immutable publication manifests
  - sparse install index documents
  - generated docs assets
- [done] Switch auth/session/token flows in `services/registry` to D1-backed reads/writes.
- [done] Switch package claims, published releases, and selector resolution cache to D1-backed reads/writes.
- [done] Move derived package web views out of R2 JSON docs and into D1-backed materialized view documents.
- [done] Expose package/homepage/owner/category/recent view documents from `api.pkgs.ml/v1/views/...`.
- [done] Update `services/pkgs.ml` to fetch lightweight views from `api.pkgs.ml` instead of `cdn.pkgs.ml`.
- [done] Keep `cdn.pkgs.ml` as the delivery surface for sparse index JSON and heavy immutable blobs only.
- [done] Update unit and e2e tests for the new DB-backed control plane.

## Follow-up

- [done] Switch `services/registry` fully to Wrangler D1 SQL migrations and remove runtime schema bootstrapping from worker request paths.
- [done] Make registry tests initialize SQLite from `services/registry/migrations/*.sql` so local schema matches remote D1 exactly.
- [done] Add publish metadata (`description`, `license`, `public`, missing versions) to all real workspace package manifests so the repo can be bulk-published in live e2e.
- [done] Add D1 backup and rollback plan for `services/registry` using Cloudflare-native Time Travel + scheduled D1→R2 exports.
- [pending] Decide whether to rename `SEARCH_DB` to a more accurate binding once the metadata migration is complete.
- [pending] Decide whether `claimKey` / `releaseKey` should become API URLs or remain compatibility-only logical ids.
