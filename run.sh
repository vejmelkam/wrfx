#!/usr/bin/env bash
erl -sname wrfx -mnesia dir db -pa deps/*/ebin apps/*/ebin libs/*/ebin -boot start_sasl -s wrfx
