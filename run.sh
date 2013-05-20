#!/usr/bin/env bash
erl -pa apps/*/ebin libs/*/ebin -boot start_sasl
