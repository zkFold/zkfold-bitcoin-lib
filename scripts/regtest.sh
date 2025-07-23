export datadir=$(mktemp -d "/tmp/bitcoind-regtest-XXXXXX")
echo "Data directory (datadir) for storage of regtest files: $datadir"
bitcoind -regtest -datadir=$datadir -fallbackfee=0.0001000 -txindex=1 -daemon -rpcuser=user -rpcpassword=password

btc() {
    bitcoin-cli -regtest -datadir=$datadir -rpcport=18443 -rpcuser=user -rpcpassword=password "$@"
}

while ! btc getblockchaininfo &> /dev/null; do
    echo "Waiting for bitcoind"
    sleep 5
done

echo "bitcoind is ready"
sleep 5


PRIVKEY="tprv8ZgxMBicQKsPd66qSfNTYkdM76NsJ368nHs7r1WnKhmUbdx4Gwkhk175pvpe2A652Xzszhg2qf55w8qpRzNBwMboA3R6PoABT36eHV89dRZ"
btc createwallet "testwallet" false true > /dev/null
ORIGINAL_DESCRIPTOR="wpkh($PRIVKEY/*)"
DESCRIPTOR_INFO=$(btc getdescriptorinfo "$ORIGINAL_DESCRIPTOR")
CHECKSUM=$(echo $DESCRIPTOR_INFO | jq -r '.checksum')
FULL_DESCRIPTOR="$ORIGINAL_DESCRIPTOR#$CHECKSUM"
btc -rpcwallet=testwallet importdescriptors "[{\"desc\": \"$FULL_DESCRIPTOR\", \"timestamp\": \"now\", \"active\": true, \"internal\": false, \"range\": [0,0]}]" > /dev/null
# Address should be bcrt1qcdu42ejr7nyraa93p8x303a0gnvqz8xq9lx639
ADDRESS=$(btc -rpcwallet=testwallet getnewaddress "" "bech32")
echo "Address: $ADDRESS (derived from known extended privkey $PRIVKEY)"
echo "Funding address"
btc generatetoaddress 1 "$ADDRESS" > /dev/null
echo "Funded"

echo "Adding dummy blocks by sending funds to a dummy address such that coinbase bitcoin of our test wallet reach maturity"

mineDummyBlocks() {
  btc generatetoaddress $1 "bcrt1qs758ursh4q9z627kt3pp5yysm78ddny6txaqgw" > /dev/null
}

mineDummyBlocks 100


cleanup() {
    echo "Stopping bitcoind"
    btc stop
    echo "Stopped bitcoind, removing datadir"
    rm -rf $datadir
    echo "datadir removed"
}

trap "cleanup; exit 0" INT TERM

echo "RegTest is running. Press Ctrl+C to stop."

while true; do
  mineDummyBlocks 1
  sleep 10
done