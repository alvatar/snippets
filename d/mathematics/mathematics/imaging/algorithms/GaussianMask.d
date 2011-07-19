/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

module mathematics.imaging.algorithms.GaussianMask;


private
{
	import mathematics.imaging.MemoryImage;
	import tango.math.Math;
}


//	http://www.pages.drexel.edu/~weg22/can_tut.html			(Bill Green)

//	Pfff ...  Sclytrack wants a remote controlled plane.


MemoryImage!(T) gaussianMask5x5(T)()
{
	MemoryImage!(T) res = new MemoryImage!(T)(5,5,1);
	res[0,0,0]=2.0/115.0;res[1,0,0]=2.0/115.0;res[2,0,0]=2.0/115.0;res[3,0,0]=2.0/115.0;res[4,0,0]=2.0/115.0;
	res[0,1,0]=4.0/115.0;res[1,1,0]=9.0/115.0;res[2,1,0]=12.0/115.0;res[3,1,0]=9.0/115.0;res[4,1,0]=2.0/115.0;
	res[0,2,0]=5.0/115.0;res[1,2,0]=12.0/115.0;res[2,2,0]=15.0/115.0;res[3,2,0]=12.0/115.0;res[4,2,0]=5.0/115.0;
	res[0,3,0]=4.0/115.0;res[1,3,0]=9.0/115.0;res[2,3,0]=12.0/115.0;res[3,3,0]=9.0/115.0;res[4,3,0]=4.0/115.0;
	res[0,4,0]=2.0/115.0;res[1,4,0]=4.0/115.0;res[2,4,0]=5.0/115.0;res[3,4,0]=4.0/115.0;res[4,4,0]=2.0/115.0;
	return res;
}

	/**
	* Provides a gaussian mask.
	*
	* Params:
	*	size		=	Width and height of the image.
	*	variance	=	Variance of the Gaussian.
	*
	* Returns:
	*	Normalized 2 dimensional gaussian mask.
	*
	* See_Also
	*	http://en.wikipedia.org/wiki/Gaussian_blur
	*/

	

MemoryImage!(T) gaussianMask(T)( int size, T variance)
{
	MemoryImage!(T) result = new MemoryImage!(T) (size, size, 1);
	real center = size / 2.0;
	real xOffset, yOffset;
	real norm = 0;
	for (int y = 0; y < size; y++)
	  for (int x = 0; x < size; x++)
	  {
		xOffset = (x-center);
		yOffset = (y-center);
		radius2 = xOffset * xOffset + yOffset * yOffset;
		result[x,y] = exp(-radius2/(2.0* variance*variance)) ;
		norm += result[x,y];
	  }

	//	Normalize the result.
	for (int y = 0; y < size; y++)
	  for (int x = 0; x < size; x++)
	  {
		result[x,y] = result[x,y] / norm;
	  }
	return result;
}

	/**
	* Provides a gaussian mask but enforces a zero value for everything greater than the half of size.
	*
	* The result is a nice circular region.
	*
	* Params:
	*	size		=	Width and height of the image.
	*	variance	=	Variance of the Gaussian.
	*
	* Returns:
	*	Normalized 2 dimensional gaussian mask.
	*
	* See_Also
	*	http://en.wikipedia.org/wiki/Gaussian_blur
	*/



MemoryImage!(T) circularGaussianMask(T)(int size, T variance)
{
	MemoryImage!(T) result = new MemoryImage!(T) (size, size, 1);
	real center = size / 2.0;
	real xOffset, yOffset;
	real norm = 0;
	real radius2;
	for (int y = 0; y < size; y++)
	  for (int x = 0; x < size; x++)
	  {
		xOffset = (x-center);
		yOffset = (y-center);
		radius2 = xOffset * xOffset + yOffset * yOffset;
		if (sqrt(radius2) > center)
		{
			result[x,y] = 0;
		} else
		{
			result[x,y] = exp(-radius2/(2.0* variance*variance)) ;
			norm += result[x,y];
		}
	  }
	//	Normalize the result.
	for (int y = 0; y < size; y++)
	  for (int x = 0; x < size; x++)
	  {
		result[x,y] = result[x,y] / norm;
	  }
	return result;
}



