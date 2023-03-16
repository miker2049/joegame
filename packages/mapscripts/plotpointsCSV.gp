set terminal png size 2000,2000
set output "myplot2.png"
set xlabel "X Axis"
set ylabel "Y Axis"
set grid linecolor rgb "#404040" linewidth 1.5
set xrange [0:1000]
set yrange [1000:0]
set title "My Data"
set key off
set datafile separator ","
plot csv using 1:2:(stringcolumn(0)) with labels
