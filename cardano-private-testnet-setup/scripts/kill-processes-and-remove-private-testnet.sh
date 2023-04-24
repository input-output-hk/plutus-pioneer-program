#!/bin/sh

set -e

SCRIPT_PATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

ROOT=private-testnet

wait_until_count_of_running_nodes() {
  echo "wait until running node count is $1"
  running_nodes_cnt=$( ps -ef | grep 'cardano-node' | grep -v grep | wc -l )
  while [ $running_nodes_cnt -ne $1 ]; do
    echo "Sleep 5 secs"; sleep 5
    running_nodes_cnt=$( ps -ef | grep 'cardano-node' | grep -v grep | wc -l )
  done
  echo "running node count is $1"
}

wait_until_count_of_running_db_sync() {
  echo "wait until cardano-db-sync process count is $1"
  running_process_cnt=$( ps -ef | grep 'cardano-db-sync' | grep -v grep | wc -l )
  while [ $running_process_cnt -ne $1 ]; do
    echo "Sleep 5 secs"; sleep 5
    running_process_cnt=$( ps -ef | grep 'cardano-db-sync' | grep -v grep | wc -l )
  done
  echo "running cardano-db-sync process count is $1"
}

kill_running_nodes() {
  echo "send kill signal for each running node PID"
  for PID in `ps -ef | grep 'cardano-node' | grep -v grep |  awk '{print $2}'`;do kill -TERM $PID 2> /dev/null; done
  wait_until_count_of_running_nodes 0
}

kill_cardano_db_sync() {
  echo "send kill signal for cardano-db-sync, if applicable"
  for PID in `ps -ef | grep 'cardano-db-sync' | grep -v grep |  awk '{print $2}'`;do kill -TERM $PID 2> /dev/null; done
  wait_until_count_of_running_db_sync 0
}

kill_cardano_db_sync
echo

kill_running_nodes
echo

echo "remove private testnet directory"
rm -rf $ROOT


