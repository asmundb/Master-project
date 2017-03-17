#!/bin/bash

gfortran  -fpic -g -O2 -fstack-protector-strong  -c  load_ISBA_var.f90 -o load_ISBA_var.o -I/usr/include -L/usr/lib -lnetcdff -lnetcdf


gfortran -shared  -fpic -g -O2 -fstack-protector-strong  load_ISBA_var.o -o load_ISBA_var.so -I/usr/include -L/usr/lib -lnetcdff -lnetcdf

