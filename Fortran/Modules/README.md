# Fortran Modules
## Graphics.f90
Contains several functions. Mostly takes coordinates and other data and plots it into an image.
### Subroutine: `plot(plotIn,plotSize)`
Uses the size of `plotIn` to decide which plot subroutine to pass `plotIn` and `plotSize` to.
`plotIn(2,:)` goes to `plot2(plotIn,plotSize)`
`plotIn(3,:)` goes to `plot3(plotIn,plotSize)`
`plotIn(4,:)` goes to `plot3I(plotIn,plotSize)`
`plotIn(5,:)` goes to `plotC(plotIn,plotSize)`
`plotIn(6,:)` goes to `plotCI(plotIn,plotSize)`
### Subroutine: `plot2(plotIn,plotSize)`
Takes `plotIn` as an array of x and y inputs and puts them in a PGM Image of a size defined by `plotSize`. Image file is `./Plot Output`.
### Subroutine: `plot3(plotIn,plotSize)`
Takes `plotIn` as an array of x, y, and intensity inputs and puts them as grey values in a PGM Image of a size defined by `plotSize`. Intensity sets the grey values. Values of higher intensity will cover values of lower intensity. Image file is `./Plot Output`.
### Subroutine: `plot3I(plotIn,plotSize)`
Takes `plotIn` as an array of x, y, intensity, and importance inputs and puts them as grey values in a PGM Image of a size defined by `plotSize`. Intensity sets the grey values. Values of higher importance will cover values of lower importance. Image file is `./Plot Output`.
### Subroutine: `plotC(plotIn,plotSize)`
Takes `plotIn` as an array of x, y, Red, Green, and Blue inputs and puts them as colors in a PPM Image of a size defined by `plotSize`. Red, green, and blue Values set the color. Values of higher intensity will cover values of lower intensity. Intensity in this is the average between the red, green and blue values. Image file is `./Plot Output`.
### Subroutine: `plotCI(plotIn,plotSize)`
Takes `plotIn` as an array of x, y, Red, Green, and Blue, and importance inputs and puts them as colors in a PPM Image of a size defined by `plotSize`. Red, green, and blue Values set the color. Values of higher importance will cover values of lower importance. Image file is `./Plot Output`.
### Function: `AngleToHue(Th)`
Takes the angle `Th` and outputs one of the components of hue. The other values can be found by adding `2*pi/3` and `4*pi/3` to the input.
### Function: `linspace(st,en,leng)`
Outputs a list of values that is `leng` numbers long, starting at `st` and ending at `en`.
### Subroutine: `linspaceAll2D(listIn,listOut,S)`
Takes a list of values in `listIn` being at least size `(2,:)` and does a `linspace(st,en,leng)` function between all the values with `leng` being set as `sqrt(st^2*en^2)` times in inputed `S` value. The new list in outputed to listOut.
## Quaternion.f90
Has my Quaternion type and some operations to apply to the quaternions.
### Type: `quaternion`
Is a type with one (math) real and three imaginary values.
### Operator: `.qxq.`
Applies `QMultipyQ(Q1,Q2)` as an operator.
### Operator: `.qtq.`
Applies `QPlusQ(Q1,Q2)` as an operator.
### Function: `QMultipyQ(Q1,Q2)`
Uses quaternion multiplication rules to multiply `Q1` and `Q2` together.
### Function: `QPlusQ(Q1,Q2)`
Adds quaternions `Q1` and `Q2` together.
### Function: `XYZtoQ(XYZ)`
Takes a list of XYZ values and outputs a list of quaternion values.
### Function: `QtoXYZ(Q)`
Takes a list of quaternion values and outputs a list of XYZ values.
### Function: `absQ(Q)`
Finds the absolute value of the input quaternion.
