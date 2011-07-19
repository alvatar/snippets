/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


module mathematics.imaging.algorithms.RangeMapImage;

private
{
	import mathematics.imaging.Image;
}

	/**
	* Retrieves the maximum value in an image.
	*/

void imageFindMaximum(T) (Image!(T) image, T maximum)
{
	assert(image.getComponents() == 1, "image must have only one component");
	assert(image.getHeight() > 0, "no height");
	assert(image.getWidth() > 0, "no width");
	maximum = image[0,0,0];
	for (int y = 0; y < image.getHeight(); y++)
		for (int x = 0; x < image.getWidth(); x++)
			if (image[x,y,0] > maximum) 
				maximum = image[x,y,0];
}

	/**
	* Retrieves the minimum value in an image.
	*/

void imageFindMinimum(T) (Image!(T) image, T minimum)
{
	assert(image.getComponents() == 1, "image must have only one component");
	assert(image.getHeight() > 0, "no height");
	assert(image.getWidth() > 0, "no width");
	minimum = image[0,0,0];
	for (int y = 0; y < image.getHeight(); y++)
		for (int x = 0; x < image.getWidth(); x++)
			if (image[x,y,0] < minimum) 
				minimum = image[x,y,0];
}

	/**
	* Retrieves the maximal and minimal value in an image.
	*/

void imageFindRange(T) (Image!(T) image, out T maximum, out T minimum)
{
	assert(image.getComponents() == 1, "image must have only one component");
	assert(image.getHeight() > 0, "no height");
	assert(image.getWidth() > 0, "no width");
	minimum = image[0,0,0];
	maximum = minimum;
	for (int y = 0; y < image.getHeight(); y++)
		for (int x = 0; x < image.getWidth(); x++)
		{
			if (image[x,y,0] < minimum) 
				minimum = image[x,y,0];
			if (image[x,y,0] > maximum) 
				maximum = image[x,y,0];
	}
}

	/**
	* Set the range of the image values from 0 to 1.
	*/

void imageRangeMapZeroToOne(T)( Image!(T) image, Image!(T) mappedImage)
{
	assert( image.getComponents() == 1,"image must have exactly 1 component");
	assert( mappedImage.getComponents() == 1,"image must have exactly 1 component");
	
	assert( image.getWidth() == mappedImage.getWidth(), "width don't match");
	assert( image.getHeight() == mappedImage.getHeight(), "height don't match");

	T maximum, minimum;
	imageFindRange!(T) ( image, maximum, minimum );
	T range = maximum - minimum;
	
	for (int y = 0; y < image.getHeight(); y++)
		for (int x = 0; x < image.getWidth(); x++)
			mappedImage[x,y,0] = (image[x,y,0] - minimum) / range;
}








