# RFD0028 - Local Artifact Publishing

- Feature Name: `riot_local_artifact_publishing`
- Start Date: `2026-04-01`
- Status: `implemented`

## Summary
[summary]: #summary

This RFD changes Riot package publication from "registry fetches and republishes
a repository tarball" to "the client builds a package-root artifact locally and
uploads that artifact for publication".

It does **not** introduce artifact-only public publication.
For public packages, publication remains source-locator addressed and claim
enforced. The change is that the artifact bytes are uploaded by the client
instead of being fetched by the registry from GitHub.

The uploaded tarball becomes the immutable install artifact for the published
release. It is package-root shaped, so extracting it directly yields:

```text
riot.toml
src/...
README.md
...
```

instead of a repository snapshot like:

```text
owner-repo-<sha>/packages/std/...
```

`source_key` now has a strict meaning:

- it is the immutable install artifact for the published release
- extracting it directly must yield `riot.toml` at archive root
- installers must not need `package_subdir` to unpack it correctly

The registry remains the authority for package publication. It still validates
the uploaded artifact, enforces claims and version immutability, writes the
published release record, updates the sparse index, computes an artifact
digest, and emits lifecycle events.

## Motivation
[motivation]: #motivation

Riot's current package-publication contract has a mismatch:

- the registry stores a raw repository tarball at `source_key`
- installers and `riot-deps` consume `source_key` as if it were a package install
  artifact

That mismatch is especially visible for packages published from a subdirectory.
A release for `github.com/owner/repo/packages/std` currently extracts to
something like:

```text
~/.riot/registry/pkgs.ml/src/std/0.1.0/owner-repo-<sha>/packages/std/...
```

while the installer needs:

```text
~/.riot/registry/pkgs.ml/src/std/0.1.0/riot.toml
~/.riot/registry/pkgs.ml/src/std/0.1.0/src/...
```

This forces `pkgs-ml` and `riot-deps` to recover repository structure that should
not matter during installation:

- discover and strip the synthetic tarball root directory
- understand package `subdir`
- extract only a subtree of the archive
- reconstruct the actual package root before reading `riot.toml`

That is the wrong place to pay this complexity.

The install artifact should already be shaped like the installed package. If we
fix that upstream:

- `pkgs-ml` becomes a simple "download archive -> extract package" client
- `riot-deps` stops caring about repository archive layout
- install failures become much easier to reason about
- package publication becomes explicit about what bytes are being published

There is also a UX benefit. If `riot publish` packages the local package root
itself, it can run local preflight checks before upload and fail quickly without
waiting for the registry to rediscover obvious problems.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

### New mental model

`riot publish` should publish a package artifact, not a repository snapshot.

That means the client should:

1. find the package root locally
2. validate the package locally
3. create a package-root tarball locally
4. upload that tarball
5. ask the registry to finalize the publication

The registry then:

1. reads the uploaded tarball
2. rebuilds the canonical manifest from the tarball contents
3. validates publish rules
4. claims the package name if needed
5. writes the immutable published release record
6. updates the sparse index synchronously
7. emits publish lifecycle events

The important distinction is:

- local checks are for user experience
- server checks are for truth

The registry must never trust a client-supplied manifest or dependency list
without deriving it again from the uploaded artifact.

The current registry event pipeline still applies. A successful publish should
continue to emit:

- `package.submitted`
- `package.verified`
- `package.published`
- `package.indexed`
- `package.searchable`

The difference is only where the artifact bytes come from.

### Example

Assume a repository:

```text
riot/
  packages/std/
    riot.toml
    src/result.ml
    src/path.ml
```

Publishing `packages/std` should upload a tarball whose root is:

```text
riot.toml
src/result.ml
src/path.ml
```

not:

```text
riot-<sha>/
  packages/std/riot.toml
  packages/std/src/result.ml
  packages/std/src/path.ml
```

Then installation of `std@0.1.0` is trivial:

1. download the immutable artifact referenced by the sparse index
2. extract into:
   - `~/.riot/registry/pkgs.ml/src/std/0.1.0/`
3. read:
   - `~/.riot/registry/pkgs.ml/src/std/0.1.0/riot.toml`

No `subdir`-aware extraction is needed on the client.

### Client behavior

The local publish flow should feel like:

```text
riot publish
```

Roughly:

1. parse the local package manifest
2. validate local publish prerequisites
3. create a tarball rooted at the package directory
4. authenticate with the registry
5. upload the tarball
6. finalize publication

If validation fails locally, `riot publish` should fail before upload.
If publication fails remotely, the registry should still explain the failure in
terms of package publication rules such as:

- name claim conflicts
- version already published
- invalid dependency publication state

### Provenance

Artifact bytes are the truth of what is being published.

Source provenance such as:

- canonical locator
- repository URL
- repository subdirectory
- resolved SHA

is still useful, but it should be treated as publication metadata, not as the
shape the installer depends on.

That means a published release may still say:

- this artifact came from `github.com/owner/repo/packages/std`
- at SHA `deadbeef...`

but installers should not need to understand repository structure to install the
artifact.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Artifact contract

The artifact uploaded by the client is a gzipped tar archive whose root is the
package root.

The archive must contain at least:

- `riot.toml`
- package source files

The archive must not assume an extra repository root prefix.

More specifically, the registry must accept only archives that satisfy all of
the following:

- `riot.toml` exists as a regular file at archive path `riot.toml`
- package files are rooted directly under the archive root
- no archive entry path is absolute
- no archive entry path contains `..` segments after normalization
- no duplicate normalized paths exist
- symlinks and hardlinks are rejected for the initial implementation

That last point is important. Installers should be able to implement "download
archive -> extract archive" without re-implementing tarball security policy.
The registry should centralize that validation once.

The registry may normalize archive metadata while persisting the published
artifact, for example:

- normalized file ordering
- normalized file modes
- normalized mtimes

However, that normalization must preserve the package root shape and the file
contents the manifest is derived from.

This RFD defines one canonical byte stream for the published artifact:

- if the registry normalizes the archive, the normalized stored bytes are the
  published artifact
- if the registry does not normalize the archive, the uploaded bytes are the
  published artifact

All registry-facing identifiers must refer to that same canonical byte stream:

- `source_key` stores that artifact
- `manifest_key` is derived by reading that artifact
- `source_digest` is computed from that artifact

There must not be separate digests or identities for "uploaded bytes" and
"stored bytes" in the initial implementation.

This means `source_key` in sparse-index release metadata changes meaning
slightly:

- before: immutable repository/source snapshot
- after: immutable package install artifact for the published release

This is a better contract for the client, because `source_key` now directly
means "download this to install the package".

`manifest_key` should continue to be derived from the uploaded artifact itself,
not from client-supplied metadata. For this model to stay coherent:

- `source_key` is the install artifact
- `manifest_key` is the canonical registry view of that artifact

Both must describe the same bytes.

### Local preflight checks

The client should perform lightweight local checks before upload:

- manifest parses
- `package.public = true`
- `package.name` is present
- `package.version` is semver-valid
- `package.description` is present
- `package.license` is present and syntactically valid

These checks are purely for fast feedback.
The registry must rerun publish validation after upload.

Local publish does not need to solve dependency compatibility or lock the
workspace. Those concerns remain in `riot-deps`.

Local publish also does not solve workspace ordering by itself. If a workspace
contains packages that depend on one another, `riot publish` still needs to
publish them in a valid order or fail clearly when the dependency graph is not
yet publishable.

### Proposed publish protocol

The registry should expose an upload/finalize flow rather than requiring the
registry worker to fetch a repository tarball itself.

One reasonable shape is:

1. `POST /v1/publications/start`
   - authenticate the actor
   - accept canonical source locator and requested selector/ref
   - return a `publication_id`
   - return an upload target or signed upload URL

2. upload tarball
   - client uploads the package-root tarball bytes

3. `POST /v1/publications/<publication_id>/finalize`
   - registry reads the uploaded bytes
   - derives the canonical manifest from the artifact
   - runs publish validation
   - writes package claim and published release records
   - updates the sparse index
   - emits lifecycle events

The registry may also choose a simpler single-request implementation initially,
but the conceptual model should remain upload-then-finalize.

Even in the multi-step protocol, public package publication remains
source-locator addressed. The publication session is scoped to a canonical
source locator and not just to an arbitrary uploaded tarball.

The important storage rule is:

- uploaded bytes are staged first
- no staged upload is visible as a published release
- immutable `source_key` is assigned only after finalize succeeds

That avoids conflating:

- "a client uploaded some bytes"
- "the registry accepted and published a release"

For an initial implementation, the single-request path can still use the
current publish route shape:

`POST /v1/packages/<locator>/publish?ref=<selector>`

with:

- `Content-Type: application/gzip`
- request body = package-root tarball

That route shape is intentional. It makes the policy explicit:

- the URL identifies the public package provenance and claim context
- the body provides the package-root install artifact

Even in that simplified version, the server should conceptually treat the bytes
as staged until validation completes and the release record is committed.

### Publication metadata

The client may provide provenance metadata alongside the upload:

- canonical source locator
- resolved source SHA
- repository URL
- repository subdirectory

For public package publication, the canonical source locator is required.
That keeps package-name claims tied to a stable source identity and preserves
the package-page/search/index metadata the current registry already expects.

The registry should store this provenance as metadata on the release record.
However, the registry should not require repository re-fetch in order to publish
the artifact.

The right split is:

- artifact bytes are the install truth
- canonical locator is the claim/provenance truth
- resolved SHA and repository URL are strong metadata when available, but they
  are not required for installation

If Riot later wants fully artifact-only publication, that should be introduced
as a separate explicit mode rather than silently weakening the current
source-based claim model.

### Sparse index contract

The sparse index continues to publish release metadata including:

- version
- manifest key
- source key
- source digest
- description
- license
- keywords
- dependencies
- provenance metadata such as repository and SHA

The only semantic change is that `source_key` now references the package-root
artifact directly.

The registry should compute a digest such as `sha256` from the published
artifact bytes during finalize and store it in:

- the published release record
- the sparse index release entry

That gives lockfiles, caches, and future audit tooling a stable byte-level
identity for the published artifact.

That means package managers and other clients may implement installation as:

1. fetch sparse index document
2. choose release
3. download `source_key`
4. extract into local cache root

### Cache layout

This proposal works naturally with the Cargo-style cache layout from
`RFD0026`:

```text
~/.riot/registry/pkgs.ml/index/...
~/.riot/registry/pkgs.ml/archive/<package>/<version>.tar.gz
~/.riot/registry/pkgs.ml/src/<package>/<version>/...
```

The client downloads the published package artifact into `archive/` and extracts
it directly into `src/`.

### Server responsibilities remain

The registry still owns:

- authentication
- package-name claim enforcement
- version immutability
- archive validation and normalization
- dependency publication checks
- canonical published release records
- sparse index updates
- lifecycle events

The registry should also own cleanup of abandoned staged uploads when the
multi-step protocol is used.

This RFD does not move publication authority to the client.
It only moves artifact construction to the client.

## Drawbacks
[drawbacks]: #drawbacks

- `riot publish` becomes more complex because it now needs to build and upload
  the package artifact itself.
- the registry API gains upload/session semantics instead of a simpler
  "publish this source locator" interface.
- provenance is no longer proven solely by the registry re-fetching the source
  from GitHub at publish time
- future auditing may want stronger artifact-attestation mechanisms if local
  artifact construction becomes the default path

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Keep the current repository tarball model

This was the current behavior. It makes the installer and package manager pay
for repository-structure recovery.

That is the wrong tradeoff. Installation should be simple and package-shaped.

### Keep repository tarballs, but teach clients to extract subdirectories

This is possible, and `pkgs-ml` has already started moving in that direction.
It is also exactly the kind of workaround this RFD is trying to avoid.

It couples every client to:

- tarball root conventions
- repository layout details
- package `subdir`

That complexity belongs on the publication side instead.

### Registry repacks repository tarballs itself

This is a better design than the status quo and may still be a useful fallback.
However, it keeps the registry in charge of fetching repository snapshots and
materializing artifacts from source locators.

Local artifact publishing is still cleaner when `riot publish` already has the
package on disk and can package it directly.

### Artifact-only publish without provenance

This is tempting because it is very simple.

However, provenance metadata is useful for:

- package pages
- debugging
- source browsing
- future trust and audit features

So this RFD keeps provenance as metadata, while making the package artifact the
thing installers actually consume.

It also keeps package claims coherent with the current registry model, where a
published package name is associated with a canonical source locator.

## Prior art
[prior-art]: #prior-art

- Cargo publishes crate tarballs whose roots are package-shaped, and clients
  install from those artifacts rather than from raw repository snapshots. The
  registry contract is artifact-first, and the uploaded bytes are what gets
  installed.
- Hex also publishes package artifacts, even though its local dependency layout
  is shaped by Mix. Publication normalizes the install artifact boundary so
  clients do not have to reconstruct local workspace structure.
- Riot's own sparse index already assumes install clients can fetch one release
  artifact and extract it locally.

The main lesson is that published install artifacts should be shaped for
installation, not for source-host mirroring.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should the upload protocol use signed R2 uploads, worker-streamed uploads, or
  a simpler synchronous request first?
- Should local preflight eventually include optional `riot build` or
  `riot test` gates before upload?
- Do we want a separate operator/debug path that can still publish from a
  source locator without a local artifact upload?

The question of whether public package publication requires a canonical source
locator is resolved by this RFD: it should.

## Future possibilities
[future-possibilities]: #future-possibilities

- attach signatures or attestations to local publish artifacts
- support reproducible repacking from known source locators as an audit feature
- support detached source bundles and docs bundles alongside the install
  artifact
- add local dry-run publication that builds the artifact and runs full checks
  without uploading anything
