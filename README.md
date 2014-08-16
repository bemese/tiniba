TINIBA
===========================
TINIBA is a tool written in bash, perl, and fortran to do ab initio calculations of optical responses based on the popular ABINIT.

![](https://raw2.github.com/roguephysicist/tiniba-manual/master/plots/3drho.png)

Work in Progress
-------------------
See TODO.md for current work.

This is my rewrite of the official 2.0 release and will continue to be improved upon as time allows. New features and fixes are added into the 'work' branch and are then tested in the 'testing' branch. Full documentation for the project is located in the tiniba-manual repository. It is a work in progress but will allow the average user to get up and running without too many problems.

I suggest adding these lines to the appropriate shell file:

```bash
export TINIBA=$HOME/tiniba
export PATH="$TINIBA/clustering:$TINIBA/utils:$PATH"
```

For more information you can contact me at sma@cio.mx and I will be happy to answer any questions.

References and Acknowlegdements
--------------

This project was built by the PRONASIS group of the Centro de Investigaciones en Óptica, A.C. in Mexico. It has been a collaborative project spanning almost a decade. TINIBA has been used in (at least) the following articles:
* J Opt Soc Am B 28 1882 (2011)
* Mod Phys Lett B 24, 1507 (2010)
* Opt Laser Eng 49, 668 (2011)
* Opt Mater 29, 1 (2006)
* Phys Rev B 73, 195330 (2006)
* Phys Rev B 74, 075318 (2006)
* Phys Rev B 76, 205113 (2007)
* Phys Rev B 79, 245132 (2009)
* Phys Rev B 80, 155205 (2009)
* Phys Rev B 80, 201312 (2009)
* Phys Rev B 80, 245204 (2009)
* Phys Rev B 84, 165316 (2011)
* Phys Rev B 84, 195326 (2011)
* Phys Rev B 85, 165324 (2012)
* Phys Status Solidi B 242, 3022 (2005)
* Phys Status Solidi B 247, 1979 (2010)
* Phys Status Solidi C 8, 2604 (2008)
* Phys Status Solidi C 9, 1378 (2012)
* Surf Sci 605, 941 (2011)
