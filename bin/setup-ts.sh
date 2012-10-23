#!/bin/bash
export TS_HOME="${HOME}/opt/typesafe-stack"

echo "Adding ${TS_HOME} to the path"
export PATH="${TS_HOME}/bin:${PATH}"

echo "Setting SBT_HOME to ${TS_HOME}/bin"
export SBT_HOME="${TS_HOME}/bin"

$SHELL
