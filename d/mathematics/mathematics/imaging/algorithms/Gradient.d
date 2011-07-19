/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/



module mathematics.imaging.algorithms.Gradient;

private
{
	import mathematics.imaging.Image;
	import tango.math.Math;
}

	/**
	* Finds the gradient of the image in the x direction.
	*
	* Params:
	*	sourceImage	=	The original image
	*	gradientImage	=	GradientImage that must have a width of sourceWidth - 1
	*/



void imageGradientX(T) (Image!(T) sourceImage, Image!(T) gradientImage)
{
	assert( sourceImage.getComponents() == 1, "sourceImage must have 1 component");
	assert( gradientImage.getComponents() == 1, "gradientImage must have 1 component");
	assert( gradientImage.getWidth() >= sourceImage.getWidth() -1, "gradientImage width not enough");
	assert( gradientImage.getHeight() >= sourceImage.getHeight(), "gradientImage height not enough");
	for (int y = 0; y < sourceImage.getHeight(); y++)
		for (int x = 0; x < sourceImage.getWidth()-1; x++)
			gradientImage[x, y, 0] = sourceImage[x+1,y, 0] - sourceImage[x, y, 0];	
}

	/**
	* Finds the gradient of the image in the y direction.
	*
	* Params:
	*	sourceImage	=	The original image
	*	gradientImage	=	GradientImage that must have a height of sourceHeight - 1
	*/


void imageGradientY(T)( Image!(T) sourceImage, Image!(T) gradientImage)
{
	assert( sourceImage.getComponents() == 1, "sourceImage must have 1 component");
	assert( gradientImage.getComponents() == 1, "gradientImage must have 1 component");
	assert( gradientImage.getWidth() >= sourceImage.getWidth(), "gradientImage width not enough");
	assert( gradientImage.getHeight() >= sourceImage.getHeight()-1, "gradientImage height not enough");
	for (int x = 0; x < sourceImage.getWidth(); x++)
		for (int y = 0; y < sourceImage.getHeight()-1; y++)
			gradientImage[x, y, 0] = sourceImage[x, y+1,0] - sourceImage[x, y,0];
}


	/**
	* Finds the magnitude of the gradient of an image.
	*
	* All images must of of the same dimensions.
	*
	* Params:
	*	gradientX	=	The gradient x image
	*	gradientY	=	The gradient y image
	*	gradientImage	=	Magnitude of the gradient.
	*/



void combinedGradient(T) (Image!(T) gradientX, Image!(T) gradientY, Image!(T) gradientImage)
{
	assert( gradientX.getComponents() == 1, "gradientX components must be 1");
	assert( gradientY.getComponents() == 1, "gradientY components must be 1");
	assert( gradientImage.getComponents() == 1, "gradientImage components must be 1");


	

	assert( gradientX.getWidth() == gradientImage.getWidth(), "width of gradientX does not match");
	assert( gradientY.getWidth() == gradientImage.getWidth(), "width of gradientY does not match");

	assert( gradientX.getHeight() == gradientImage.getHeight(), "height of gradientX does not match");
	assert( gradientY.getHeight() == gradientImage.getHeight(), "height of gradientY does not match");

	for (int y = 0; y < gradientImage.getHeight(); y++)
		for (int x = 0; x < gradientImage.getWidth(); x++)
			gradientImage[x,y,0] = sqrt(gradientX[x, y, 0] * gradientX[x, y, 0] + gradientY[x,y,0] * gradientY[x,y,0]);

}


