#!/bin/bash
P=$PWD
rsh itanium01 "cd $P; make -f Makefile64b clean; make -f  Makefile64b" 
