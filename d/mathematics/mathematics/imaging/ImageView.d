/**
 * Image View.
 * 
 * Copyright: 	Copyright (C) 2006-2007 Peter Van Isacker
 * Authors: 	Peter Van Isacker (sclytrack)
 * Date:		June 20, 2006
 * License:	Copyright (C) 2006-2007 Peter Van Isacker
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 *	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *	version:	initial
 */

module mathematics.imaging.ImageView;




private {
	import mathematics.imaging.Color;
	import mathematics.imaging.Image;
	import mathematics.imaging.MemoryImage;
}

/*
	Images are presented in the following manner, not taken into account
	the amount of components.

	Coordinates	Memory
	[0,0]		[0]
	[width,0]	[width]	
*/




//-----------------------------------------
//	View
//-----------------------------------------




/*


struct View
{
public:
	int x;
	int y;
	int width;
	int height;

	static View opCall(int x, int y, int width, int height)
	{
		View result;
		result.x = x;
		result.y = y;
		result.width = width;
		result.height = height;
		return result;
	}
}


	//
	// This is an abstract base class of the different image views.
	//
	// See_Also:
	//  ConvertImageView
	//

class ImageView(T, int COMPONENTS): Image!(T)
{
protected:

	//
	// Protected constructor of the current ImageView
	// PARAMS:
	//	height		= height of the current view
	//	width		= width of the current view
	//	sourceImage	= image this view is referencing
	//

	int _imageWidth;
	int _imageHeight;
	int _imageComponents;	

	this(int width, int height)
	{
		assert( width > 0, "width > 0 failed");
		assert( height > 0, "height > 0 failed");
		_imageWidth = width;
		_imageHeight = height;
	}
public:
	int getWidth()
	{
		return _imageWidth;
	}

	int getHeight()
	{
		return _imageHeight;
	}

	int getComponents()
	{
		return COMPONENTS;
	}

	//
	// Instantiates a new copy of the image.
	//

	MemoryImage!(T) newMemoryImage()
	{
		static if (COMPONENTS == 1)
		{
			MemoryImage!(T) result = new MemoryImage!(T) (getWidth(), getHeight(), getComponents());
			for (int y = 0; y < getHeight(); y++)
				for (int x = 0; x < getWidth(); x++)
					result[x,y] = this[x,y];
			return result;
		}
		else
		{
			MemoryImage!(T) result = new MemoryImage!(T) (getWidth(), getHeight(), getComponents());
			for (int y = 0; y < getHeight(); y++)
				for (int x = 0; x < getWidth(); x++)
					for (int c = 0; c < getComponents(); c++)
						result[x,y,c] = this.opIndex(x,y,c);
			return result;
		}		
	}
}

*/


	
	
		


