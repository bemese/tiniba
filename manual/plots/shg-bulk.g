## metapost
set term mp solid color  latex magnification 1
set out 'fig.mp'
#set multiplot
# set size 1.4,1.4
#
 set xrange [0:5]
#
 set ylabel "\\Large Im[$\\chi_{xyz}$] (pm/V)" 
 set xlabel "\\Large photon-energy (eV)"
p 'data/shgL.sm_xyz_1661_20-nospin_scissor_1.051_Nc_7' u 1:($3+$5) t "L" w l lt 1 lw 8,\
  'data/shgV.sm_xyz_1661_20-nospin_scissor_1.051_Nc_7' u 1:($3+$5) t "V" w l lt 2 lw 1

#
