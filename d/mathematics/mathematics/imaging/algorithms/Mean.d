/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


module mathematics.imaging.algorithms.Mean;

private
{
	import mathematics.imaging.Image;
	import mathematics.imaging.ImageView;
}

T imageMean(T) (Image!(T) image)
{
	assert( image.getComponents == 1, "Components must be 1");
	T mean = 0;
	for (int y = 0; y < image.getHeight(); y++)
		for (int x = 0; x < image.getWidth; x++)
			mean += image[x,y];
	return mean / (image.getWidth() * image.getHeight());				
}


T imageViewMean(T) (Image!(T) image, View view)
{
	assert( image.getComponents == 1, "Components must be 1");

	T mean = 0;
	for (int y = view.y; y < view.height; y++)
		for (int x = view.x; x < view.width; x++)
			mean+=image[x,y];
	return mean / (view.width * view.height);
}




	
