export datadir=$(mktemp -d "/tmp/bitcoind-regtest-XXXXXX")
echo "datadir: $datadir"
bitcoind -regtest -datadir=$datadir -fallbackfee=0.0001000 -txindex=1 -daemon

btc () {
    bitcoin-cli -regtest -datadir=$datadir -rpcport=18443 "$@"
}

while ! btc getblockchaininfo &> /dev/null; do
    echo "Waiting for bitcoind"
    sleep 5
done

echo "bitcoind is ready"
sleep 5


PRIVKEY="tprv8ZgxMBicQKsPd66qSfNTYkdM76NsJ368nHs7r1WnKhmUbdx4Gwkhk175pvpe2A652Xzszhg2qf55w8qpRzNBwMboA3R6PoABT36eHV89dRZ"
btc createwallet "testwallet" false true
ORIGINAL_DESCRIPTOR="wpkh($PRIVKEY/*)"
DESCRIPTOR_INFO=$(btc getdescriptorinfo "$ORIGINAL_DESCRIPTOR")
CHECKSUM=$(echo $DESCRIPTOR_INFO | jq -r '.checksum')
FULL_DESCRIPTOR="$ORIGINAL_DESCRIPTOR#$CHECKSUM"
btc -rpcwallet=testwallet importdescriptors "[{\"desc\": \"$FULL_DESCRIPTOR\", \"timestamp\": \"now\", \"active\": true, \"internal\": false, \"range\": [0,0]}]"
# Address should be bcrt1qcdu42ejr7nyraa93p8x303a0gnvqz8xq9lx639
ADDRESS=$(btc -rpcwallet=testwallet getnewaddress "" "bech32")
echo "Address: $ADDRESS (derived from known extended privkey $PRIVKEY)"
echo "Funding address"
btc generatetoaddress 101 "$ADDRESS" > /dev/null
echo "Funded"

cleanup() {
    echo "Stopping bitcoind"
    btc stop
    echo "Stopped bitcoind, removing datadir"
    rm -rf $datadir
    echo "datadir removed"
}

trap "cleanup; exit 0" INT TERM

echo "Regtest is running. Press Ctrl+C to stop."
while true; do sleep 60; done