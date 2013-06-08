#!/usr/bin/env bash
rm -rf db
erl -sname wrfx -mnesia dir db -pa apps/*/ebin libs/*/ebin -boot start_sasl -s file eval "init/init_shodan11.erl"
