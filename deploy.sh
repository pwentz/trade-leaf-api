#! /bin/bash

set -e
echo "Building TradeLeaf..."
stack build
strip `stack exec -- which trade-leaf`
echo "Creating bundle..."
cp `stack exec -- which trade-leaf` trade-leaf
tar -czvf trade-leaf.keter trade-leaf config ql-ui/assets
rm trade-leaf
# scp ./trade-leaf.keter user@host:/opt/keter/incoming/trade-leaf.keter
rm trade-leaf.keter
