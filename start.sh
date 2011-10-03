#! /bin/bash
cd `dirname $0`
erl -pa $PWD/ebin $PWD/deps/socketio/ebin $PWD/deps/socketio/deps/misultin/ebin $PWD/deps/socketio/deps/jsx/ebin -boot start_sasl -eval "ok = application:start(misultin), ok = application:start(socketio), ok = application:start(socketio_demo)."
