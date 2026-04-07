#!/usr/bin/env bash
set -euo pipefail

install_url="${INPUT_INSTALL_URL:-https://get.riot.ml}"
add_to_path="${INPUT_ADD_TO_PATH:-true}"
riot_home="${HOME}/.riot"

if ! command -v curl >/dev/null 2>&1; then
  echo "setup-riot requires curl to be available on the runner" >&2
  exit 1
fi

mkdir -p "${riot_home}"

curl -fsSL "${install_url}" | sh -

riot_bin="${riot_home}/bin/riot"
if [[ ! -x "${riot_bin}" ]]; then
  echo "setup-riot expected riot to be installed at ${riot_bin}" >&2
  exit 1
fi

if [[ "${add_to_path}" == "true" ]]; then
  echo "${riot_home}/bin" >> "${GITHUB_PATH}"
fi

{
  echo "riot-bin=${riot_bin}"
  echo "riot-home=${riot_home}"
  echo "install-url=${install_url}"
} >> "${GITHUB_OUTPUT}"
