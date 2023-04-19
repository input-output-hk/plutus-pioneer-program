#!/bin/sh

set -e

SCRIPT_PATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

ROOT=private-testnet
export CARDANO_NODE_SOCKET_PATH=$ROOT/node-spo1/node.sock
echo "path: $CARDANO_NODE_SOCKET_PATH"

wait_until_socket_detected()
{
  echo "wait until socket is detected, socket: $CARDANO_NODE_SOCKET_PATH"
  while [ ! -S "$CARDANO_NODE_SOCKET_PATH" ]; do
    echo "Sleep 5 secs"; sleep 5
  done
  echo "socket is detected"
}

start_all_nodes() {
  echo "start all the nodes in bg"
  $ROOT/run/all.sh > /dev/null 2>&1 &
  wait_until_count_of_running_nodes 3
  echo
  echo "PIDs of started nodes:"
  for PID in `ps -ef | grep 'cardano-node' | grep -v grep |  awk '{print $2}'`;do echo "PID: $PID"; done
}

wait_until_count_of_running_nodes() {
  echo "wait until running node count is $1"
  running_nodes_cnt=$( ps -ef | grep 'cardano-node' | grep -v grep | wc -l )
  while [ $running_nodes_cnt -ne $1 ]; do
    echo "Sleep 5 secs"; sleep 5
    running_nodes_cnt=$( ps -ef | grep 'cardano-node' | grep -v grep | wc -l )
  done
  echo "running node count is $1"
}

kill_running_nodes() {
  echo "send kill signal for each running node PID"
  for PID in `ps -ef | grep 'cardano-node' | grep -v grep |  awk '{print $2}'`;do kill -TERM $PID 2> /dev/null; done
  wait_until_count_of_running_nodes 0
}

restart_nodes_in_bg()
{
  echo "Request to (re)start nodes received"
  kill_running_nodes
  start_all_nodes
  echo "Nodes restarted"
}

run_update_script()
{
  echo "request to run update-$1 script"
  if [ -z "$2" ]
  then
    "${SCRIPT_PATH}"/update-$1.sh
  else
    "${SCRIPT_PATH}"/update-$1.sh $2
  fi
  echo "Sleep 5 secs to ensure update is received"; sleep 5
  echo "update-$1 script completed"
}

query_tip()
{
  cardano-cli query tip --testnet-magic 42
}

# rerunning the script should always result in restarting nodes
# this way we can at least control this a bit more, compared to if users where to do it manually
"${SCRIPT_PATH}"/kill-processes-and-remove-private-testnet.sh

# run script to create config
"${SCRIPT_PATH}"/mkfiles.sh

restart_nodes_in_bg
echo
wait_until_socket_detected

#run_update_script "1"

#query_tip
cli_version=$(cardano-cli version)
echo "CLI Version = $cli_version"

echo
current_era=$( cardano-cli query tip --testnet-magic 42 | jq '.era' )
protocol_version=$( cardano-cli query protocol-parameters --testnet-magic 42 | jq '.protocolVersion.major' )
echo "Nodes are running in era: $current_era, major protocol version: $protocol_version"
echo
echo "Congrats! Your network is ready for use!"

wait