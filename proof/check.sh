#!/bin/sh
set -eu

proof_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
docker run --rm \
  --mount "type=bind,src=${proof_dir},dst=/proof,readonly" \
  docker.io/rocq/rocq-prover:9.2.0 \
  sh -lc 'cp /proof/CatenaKernel.v /tmp/CatenaKernel.v && cd /tmp && rocq compile CatenaKernel.v'
