#!/usr/bin/env bash

cd `dirname $0`/..

cabal sandbox init
cabal update
cabal install --only-dependencies -j
cabal build -j

bower install

cd public/vendor/zepto
npm install
npm run-script dist
