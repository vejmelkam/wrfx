#!/usr/bin/env bash
erl -sname wrfx -mnesia dir db -pa apps/*/ebin libs/*/ebin -boot start_sasl
