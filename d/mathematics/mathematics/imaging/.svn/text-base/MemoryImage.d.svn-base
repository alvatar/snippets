/**
 * Image allocated in memory.
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


module mathematics.imaging.MemoryImage;


private {
	import mathematics.imaging.Image;
	import tango.io.Stdout;
}

/**
*	Is a class that provides access to an underlying array of data.
*	They can be accessed in 3 dimensions: width, height, component.
*/
	


class ImageView(T):ImageBase!(T)
{
protected:
	T [] _data;
	uint _imageWidth;
	uint _imageHeight;
	uint _imageComponents;
	uint _widthStep;
	uint _heightStep;
	uint _componentStep;
	uint _indexOffset;
public:
        /***********************************************************************
		Creates a new Memory Image pointing to existing data or data slice.
		Use with care.

		Params:
		width	=	Width of the view
		height	=	Height of the view
		components =	Components of the view, usually 1, 3
		widthStep =	Usually equal to the amount of components
		heightStep =	Usually width * components
		componentStep = Usually 1
		data		= Storage array where the data is located.
		indexOffset   = Number that is added when indexing.
			
        ***********************************************************************/

	this( uint width, uint height, uint components, uint widthStep, uint heightStep, uint componentStep, T [] data, uint indexOffset)
	{

		assert( width > 0, "width > 0 failed");
		assert( height > 0, "height > 0 failed");
		assert( components > 0, "components > 0 failed");

		_imageWidth = width;
		_imageHeight = height;
		_imageComponents = components;
		_data = data;
		_widthStep = widthStep;
		_heightStep = heightStep;
		_componentStep = componentStep;
		_indexOffset = indexOffset;
		assert( width >= 1, "width >= 1 failed");
		assert( height >= 1, "height >= 1 failed");
		assert( components >= 1, "components >= 1 failed");
		assert( data != null, "data is empty");
		assert( widthStep > 0, "widthStep > 0");
		assert( heightStep > 0, "heightStep > 0");
		assert( componentStep >= 0, "componentStep >= 0");

		assert( _data.length > (_imageWidth-1) * _widthStep + (_imageHeight-1) * _heightStep + (_imageComponents-1) * _componentStep + _indexOffset);
		
	}

        /***********************************************************************
	
		Provides a view to another ImageView

		Example:
		---
		auto view1 = new ImageView!(float)(3u,3u,2u);
		for (uint x = 0; x < view1.getWidth; x++)
			for (uint y = 0; y < view1.getHeight(); y++)
			{
				view1[x,y,0] = x;
				view1[x,y,1] = y;
			}
		Stdout( view1 ).newline;
		auto view2 = new ImageView!(float)(1u,1u,2u,2u, view1);
		Stdout(view2).newline;

		auto view3 = new ImageView!(float)(0u,0u, 2u, 1u, view2);
		Stdout(view3).newline;
		---
		
		Ouput:
		---
		[(  0.00     0.00   )(  1.00     0.00   )(  2.00     0.00   )]
		[(  0.00     1.00   )(  1.00     1.00   )(  2.00     1.00   )]
		[(  0.00     2.00   )(  1.00     2.00   )(  2.00     2.00   )]


		[(  1.00     1.00   )(  2.00     1.00   )]
		[(  1.00     2.00   )(  2.00     2.00   )]


		[(  1.00     1.00   )(  2.00     1.00   )]
		---
	
        ***********************************************************************/


	this (uint x, uint y, uint width, uint height, ImageView image)
	{	
		assert( x + width <= image.getWidth);
		assert( y + height <= image.getHeight);
		uint offset = x * image.getWidthStep + y * image.getHeightStep + image.indexOffset;
		assert( image.getData().length > (width-1) * image.getWidthStep + (height-1) * image.getHeightStep + (image.getComponents-1) * image.getComponentStep + offset);
		this(width, height, image.getComponents, image.getWidthStep, image.getHeightStep, image.getComponentStep, image.getData, offset);
	}


	/***********************************************************************

		
		Provides an easy constructor.

		Example:
		---		
		auto image = new ImageView!(float)(10u, 8u, 3u);
		---

	***********************************************************************/

	this (uint width, uint height, uint components)
	{
		assert( width >= 1, "width >= 1 failed");
		assert( height >= 1, "height >= 1 failed");
		assert( components >= 1, "components >= 1 failed");
		T [] data = new T[height*width*components];
		this( width, height, components, components, width * components, 1, data, 0);
	}

	/**
	*	This is the data the memory image uses.
	*/

	T [] getData()
	{
		return _data;
	}

	/**
	*	Provides access to the elements of the memory image;
	*/



	/*
	override T opIndex(uint x, uint y)
	{
		assert( getComponents() == 1, "The image must have 1 component only");
		assert( x >= 0, "x is negative");
		assert( y >= 0, "y is negative");
		assert( x < getWidth(), "x is larger than the width");
		assert( y < getHeight(), "y is larger than the height");
		return _data[ y * _heightStep + x * _widthStep + _indexOffset];
	}
	*/

	/**
	*	Provides a means to assign values to elements of a memory image.
	*/

	/*
	override void opIndexAssign(T value, uint x, uint y)
	{
		assert( getComponents() ==1, "The image must have 1 component only");
		assert( x >= 0, "x is negative");
		assert( y >= 0, "y is negative");
		assert( x < getWidth(), "x is larger than the width");
		assert( y < getHeight(), "y is larger than the height");
		_data[ y * _heightStep + x * _widthStep + _indexOffset] = value;
	}
	*/

	override T opIndex(uint x, uint y, uint component)
	{
		assert( x >= 0, "x is negative");
		assert( y >= 0, "y is negative");
		assert( component >= 0, "component is negative");
		assert( x < getWidth(), "x is larger than the width");
		assert( y < getHeight(), "y is larger than the height");
		assert( component < getComponents(), "component out of bounds");
		return _data[y * _heightStep + x * _widthStep + component * _componentStep + _indexOffset];
	}


	override void opIndexAssign(T value, uint x, uint y, uint component)
	{
		assert( x >= 0, "x is negative");
		assert( y >= 0, "y is negative");
		assert( component >= 0, "component is negative");
		assert( x < getWidth(), "x is larger than the width");
		assert( y < getHeight(), "y is larger than the height");
		assert( component < getComponents(), "component out of bounds");

		_data[y * _heightStep + x * _widthStep + component * _componentStep + _indexOffset] = value;
	}

	/**
	*	Returns the width of the image.
	*/

	uint getWidth()
	{
		return _imageWidth;
	}

	uint getHeight()
	{
		return _imageHeight;
	}

	uint getComponents()
	{
		return _imageComponents;
	}

	/**
	*	Step count to the next component
	*/

	uint getComponentStep()
	{		
		return _componentStep;
	}

	/**
	*	Width step to the next element.
	*/


	uint getWidthStep()
	{
		return _widthStep;		
	}

	/**
	*	Height step to the next element.
	*/


	uint getHeightStep()
	{
		return _heightStep;
	}


	/**
	*	Creates a new MemoryImage, which isn't exactly a clone.
	*/

	ImageView newImageView()
	{
		ImageView result = new ImageView( getWidth(), getHeight(), getComponents());
		/*
		if (getComponents() == 1)
		{
			for (uint y = 0; y < getHeight() ; y++)
				for (uint x = 0; x < getWidth(); x++)
					result[x,y] = this[x,y];

		} else
		{
		*/

			for (uint y = 0; y < getHeight() ; y++)
				for (uint x = 0; x < getWidth(); x++)
					for (uint c = 0; c < getComponents(); c++)
						result[x,y,c] = this[x,y,c];

		//}
		return result;
	}

	uint indexOffset()
	{
		return _indexOffset;
	}

	override char [] toString()
	{
		char [] result = "\n";
		for (uint h  = 0; h < getHeight; h++)
		{
			result ~= "[";
			for (uint w = 0; w < getWidth(); w++)
			{
				result ~= "(";
				for (uint c = 0; c < getComponents; c++)
					result ~= "  " ~tango.text.convert.Float.toString(opIndex(w, h, c)) ~ "   ";
				result~= ")";
			}
			result~="]\n";
		}
		return result;

	}
}

unittest
{
	auto view1 = new ImageView!(float)(3,3,1);
		
}


