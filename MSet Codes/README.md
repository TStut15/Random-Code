# MSet
Code for calcuating mandelbrot set.
## Code
### MSet.m
This is a Matlab Code. This runs slower than the Fortran code, but the graphical options are better for display.
### MSet.f90
The is a Fortran Code. This runs faster than the Matlab code, but can only output in the plain PGM image format.
### MSet2.f90
This is a change to the MSet.f90 code to use the other PGM format (Image is in bytes instead of Ascii) making the image file smaller.
### MSet2Color.f90
This is a change to the MSet2.f90 to add color setting the RGB to 3 out of phase cos functions.
### MSet4D.f90
I took the normal MSet calculation and using quaternions instead of regular complex numbers. This can use a lot of memory and time to calculate,
### MSetLOL.lol
I randomly decided to try to code in lolcode, and this is what I made. It only text to terminal.
## Images
Images are generated from the codes
