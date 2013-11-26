## metapost
set term mp solid color  latex magnification 1
set out 'fig.mp'
#set multiplot
# set size 1.4,1.4
#
 set hidden
 set ticslevel 0
 set grid
 set view 48,113,1
 set yrange [-8:18]
 set xrange [0:52]
 set nokey
#
 set ylabel "\\Large $z$ (a.u.)" 
 set zlabel "\\Large $\\rho_{n\\mathbf{k}}(z)$" -4,2
# set xlabel "\\Large band"
 cual=0
 set label 1 "Top" at 20,22,0
 set label 2 "Valence" at 26,22,0
 set label 3 "Band" at 33,24,0
 set arrow 1 from 26,22,0 to 26,18,0
# set zrange[0:12]
#set origin -.3,1
#sp 'rhoz_1' u (-$2-31.3852+86):($1>cual? $1:1/0):3 t "$\\Gamma$" w l
#load '/home/bms/res/ras/layer/convergence/abinit/si4h_12L/front.layers
sp 'rhoz_1' u 1:2:(($3**2+$4**2)) t "$\\Gamma$" w l

#
