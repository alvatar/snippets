/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


module mathematics.imaging.NonMaximumSuppression;


private
{
	import mathematics.imaging.Image;
}

//Gosh my computer vision book has a totally different algorithm for this one.


	/**
	* Set everything above a threshold to zero.
	*/


void nonMaximumSuppression(T) (Image!(T) image, T threshold)
{
	assert( image.getComponents() == 1, "image must have only one component");
	for (int y= 0; y < image.getHeight(); y++)
		for (int x = 0; x < image.getWidth(); x++)
			if (image[x,y,0] < threshold) image[x,y,0] = 0;
}



	




