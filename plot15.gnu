set key top left

set title "replication number R=1.5"
set ylabel "population"
set xlabel "steps (incubation time)"

set yrange [0:100000]
#set xrange [0:25]
plot "virus15.dat"  u 1:2 t "simple: new infected"\
,"" u 1:3 t "simple: accumulated" \
,"" u 1:4 t "limited: new infected" \
,"" u 1:5 t "limited: accumulated" 

