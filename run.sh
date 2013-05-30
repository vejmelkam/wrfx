#!/usr/bin/env bash
erl -mnesia dir db -pa apps/*/ebin libs/*/ebin -boot start_sasl -s wrfx
