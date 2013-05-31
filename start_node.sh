#!/usr/bin/env bash
erl -detached -sname wrfx -mnesia dir db -pa apps/*/ebin libs/*/ebin -boot start_sasl -s wrfx
