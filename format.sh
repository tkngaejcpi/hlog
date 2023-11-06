#!/usr/bin/bash
ormolu --mode inplace $(find . -wholename './src/*.hs')