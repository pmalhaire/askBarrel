#!/bin/sh
echo build server
./build.sh server
echo build client
./build.sh once

tmux new-session -d "\
echo run server;\
./bin/server"
tmux split-window -v "\
sleep 2;\
echo wait server;\
sleep 3;\
echo send command;\
./bin/once;\
echo command sent look up;\
echo closing in 5s;\
sleep 5;\
tmux kill-window -t 0"
tmux -2 attach-session -d
