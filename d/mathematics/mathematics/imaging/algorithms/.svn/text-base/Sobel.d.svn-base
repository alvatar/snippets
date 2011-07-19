/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


	http://en.wikipedia.org/wiki/Sobel
*/



module mathematics.imaging.algorithms.Sobel;



private
{
	import mathematics.imaging.Image;
}


/*
	Sobel X

	-1	0	1
	-2	0	2
	-1	0	1
*/

/*
	Sobel Y

	1	2	1
	0	0	0
	-1	-2	1
*/


//	Warning I'm taking my own rules with regard to the Y axis, it is pointed downwards.
//	yet for the gradient stuff it's pointed upwards again. Maybe I should swap it again.

	

void sobelGradientX(T)(Image!(T) sourceImage, Image!(T) gradientImage)
{
	assert( sourceImage.getComponents() == 1, "sourceImage components must be 1");
	assert( gradientImage.getComponents() == 1, "gradientImgage components must be 1");

	assert( sourceImage.getWidth() >= 3, "sourceImage not large enough");
	assert( sourceImage.getHeight() >= 3, "sourceImage not large enough");
	
	assert( gradientImage.getWidth() > sourceImage.getHeight() -3, "gradient image width not large enough");
	assert( gradientImage.getHeight() > sourceImage.getHeight() -3, "gradient image height not large enough");


	for (int y = 0; y < sourceImage.getHeight()-2; y++)
	{
		for (int x = 0; x < sourceImage.getWidth()-2; x++)
		{
			gradientImage[x,y,0]=-sourceImage[x,y,0]
					-2*sourceImage[x,y+1,0]
					-sourceImage[x,y+2,0]
					+sourceImage[x+2,y,0]
					+2*sourceImage[x+2,y+1,0]
					+sourceImage[x+2,y+2,0];
		}
	}
}


void sobelGradientY(T)(Image!(T) sourceImage, Image!(T) gradientImage)
{
	assert( sourceImage.getComponents() == 1, "sourceImage components must be 1");
	assert( gradientImage.getComponents() == 1, "gradientImgage components must be 1");

	assert( sourceImage.getWidth() >= 3, "sourceImage not large enough");
	assert( sourceImage.getHeight() >= 3, "sourceImage not large enough");

	assert( gradientImage.getWidth() > sourceImage.getHeight() -3, "gradient image width not large enough");
	assert( gradientImage.getHeight() > sourceImage.getHeight() -3, "gradient image height not large enough");


	for (int y = 0; y < sourceImage.getHeight()-2; y++)
	{
		for (int x = 0; x < sourceImage.getWidth()-2; x++)
		{
			gradientImage[x,y,0]=-sourceImage[x,y+2,0]
					-2*sourceImage[x+1,y+2,0]
					-sourceImage[x+2,y+2,0]
					+sourceImage[x,y,0]
					+2*sourceImage[x+1,y,0]
					+sourceImage[x+2,y,0];
		}
	}
}






