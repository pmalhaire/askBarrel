#!/bin/bash
./build.sh server
./build.sh fork

set -e

TMP_DIR=/tmp/askBarrel_$RANDOM
TEST_FILE=$TMP_DIR/test_file

mkdir -p $TMP_DIR
touch $TEST_FILE

for i in {1..100};do echo test_$i >> $TEST_FILE; done
echo quit >> $TEST_FILE

./bin/server > $TMP_DIR/server.log &
PID_SERVER=$!
echo "Server launched"
sleep 1
echo "Parallel clients ..."
for i in {1..100};do ./bin/fork < $TEST_FILE > $TMP_DIR/$i.log; echo -ne "\r\t$i"; done
echo -e "\n...OK"
kill $PID_SERVER

exit 1

echo "clean up"
rm $TEST_FILE
rm $TMP_DIR/*.log
rmdir $TMP_DIR

exit 0

