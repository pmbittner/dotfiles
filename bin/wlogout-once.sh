#!/usr/bin/env bash

# Helper script to launch wlogout.
# This scripts runs wlogout only if it is not already running.

if pgrep -x wlogout >/dev/null; then
    pkill -x wlogout
    exit 0
fi

wlogout
