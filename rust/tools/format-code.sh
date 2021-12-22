#!/usr/bin/env bash
rustfmt $(find . -not -path './target/*' -name '*.rs')
