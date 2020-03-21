#! /usr/bin/sh

binary='./virus'

$binary 1.3 100000 25 10 > virus13.dat
head -n 1 virus13.dat
$binary 1.5 100000 30 10 > virus15.dat
head -n 1 virus15.dat
$binary 2.0 100000 25 10 > virus20.dat
head -n 1 virus20.dat
$binary 2.5 100000 25 10 > virus25.dat
head -n 1 virus25.dat
$binary 3.0 100000 25 10 > virus30.dat
head -n 1 virus30.dat
