TINIBA
===========================
A program written in bash and fortran to do ab initio calculations of optical responses based on the popular ABINIT.

This program was written by the PRONASIS group of the Centro de Investigaciones en Óptica, A.C. in Mexico. Contact info: sma@cio.mx

This includes the official 2.0 release (along with two older versions), which is the starting point of my full rewrite of the program.

I suggest adding these lines to the appropriate shell file:

```bash
export TINIBA=$HOME/tiniba
export PATH="$TINIBA/clustering:$TINIBA/utils:$PATH"
```
