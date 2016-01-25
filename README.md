# CubicSpline3D #

## Desciption

1,2,3,4-dimentional parametric cubic spline interpolation.

## Usage 

import Linear.V3
import Math.CubicSpline3D

let points = [(t, V3 (cos t) (sin t), t) | t <- [0, pi/4 .. 4 * pi]]
let sp = spline points
let anser = map (sample sp ) [0, pi/100 .. 4* pi]

	
## TODO ##

	* Test
	* Generalize
	* Loop
    * Documents
