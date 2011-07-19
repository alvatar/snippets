/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


module mathematics.imaging.algorithms.Correlation;

private
{
	import mathematics.imaging.Image;
}


	/**
	* Performs a correlation wiht the sourceImage.
	*	
	* Params:
	*	sourceImage	= The sourceImage getComponents() must return 1
	*	filter		= The filter mask.
	*	destination	= Writes to the destination image. The size of the destination must be >= source - filter + 1
	*/

void imageCorrelation(T) (Image!(T) sourceImage, Image!(T) filter, Image!(T) destination)
{
	assert(destination.getHeight() >=  sourceImage.getHeight() - filter.getHeight()+1, "destination height not large enough");
	assert(destination.getWidth() >=  sourceImage.getWidth() - filter.getWidth()+1, "destination width not large enough");
	assert(destination.getComponents() == 1, "destination components must be 1");
	assert(sourceImage.getComponents() == 1, "sourceImage components must be 1");
	assert(filter.getComponents() == 1, "filter components must be 1");

	

	T intermediate;
	for (int y = 0; y < sourceImage.getHeight()-filter.getHeight()+1; y++)
		for (int x = 0; x < sourceImage.getWidth()-filter.getWidth()+1; x++)
		{
			intermediate = 0;
			for (int yf = 0; yf < filter.getHeight(); yf++)
				for (int xf = 0; xf < filter.getWidth(); xf++)
					intermediate += sourceImage[x+xf,y+yf] * filter[xf,yf];
			destination[x,y] = intermediate;
		}
			
}


/*
void imageSimilarity(Image!(T) image1, Image!(T) image2, T mean1, T mean2,
			VectorFixed!(T) vector1, VectorFixed!(T) vector2,
			T windowSize)
{
	
}
*/



