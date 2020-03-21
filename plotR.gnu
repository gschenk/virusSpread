#! /usr/local/bin/gnuplot
set style data linespoints

set title "Accumulated and present number of infected in 100k people for different replication numbers R. Assuming a base replication number of 3.0. Lower R values\ncorrespond to measures to limit spread. Number of initially infected 10. Discrete steps of 5.6 days assumed average incubation time."

set key top left
set ylabel "population in thousands"
set xlabel "days since 10 infected"
set yrange [0:100]

plot \
  "virus13.dat"  u ($1*5.6):($4/1000) w l ls 5 lw 2 t "R=1.3"\
, "virus15.dat"  u ($1*5.6):($4/1000) w l ls 1 lw 2 t "R=1.5"\
, "virus20.dat"  u ($1*5.6):($4/1000) w l ls 2 lw 2 t "R=2.0"\
, "virus25.dat"  u ($1*5.6):($4/1000) w l ls 3 lw 2 t "R=2.5"\
, "virus30.dat"  u ($1*5.6):($4/1000) w l ls 4 lw 2 t "R=3.0"\
, "virus13.dat"  u ($1*5.6):($5/1000) w l ls 5 lw 2 t ""\
, "virus15.dat"  u ($1*5.6):($5/1000) w l ls 1 lw 2 t ""\
, "virus20.dat"  u ($1*5.6):($5/1000) w l ls 2 lw 2 t ""\
, "virus25.dat"  u ($1*5.6):($5/1000) w l ls 3 lw 2 t ""\
, "virus30.dat"  u ($1*5.6):($5/1000) w l ls 4 lw 2 t ""\

