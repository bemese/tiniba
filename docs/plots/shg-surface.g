## metapost
set term mp solid color  latex magnification 1
set out 'fig.mp'
#set multiplot
# set size 1.4,1.4
#
 set xrange [0:5]
#
 set ylabel "\\Large Im[$\\chi_{ijk}$] (pm/V)" 
 set xlabel "\\Large photon-energy (eV)"
set key bottom
p 'data/shgC.sm_zzz_64_half-slab_5-nospin_scissor_0_Nc_26' u 1:($3+$5) t "$zzz$" w l,\
  'data/shgC.sm_zxx_64_half-slab_5-nospin_scissor_0_Nc_26' u 1:($3+$5) t "$zxx$" w l,\
  'data/shgC.sm_xxz_64_half-slab_5-nospin_scissor_0_Nc_26' u 1:($3+$5) t "$xxz$" w l,\
  'data/shgC.sm_xxx_64_half-slab_5-nospin_scissor_0_Nc_26' u 1:($3+$5) t "$xxx$" w l
#
