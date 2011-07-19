/**
 * BilinearInterpolationImage.
 * 
 * Copyright: 	Copyright (C) 2006-2007 Peter Van Isacker
 * Authors: 	Peter Van Isacker (sclytrack)
 * Date:		September 13, 2007
 * License:	Copyright (C) 2006-2007 Peter Van Isacker
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 *	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *	version:	initial
 */



module mathematics.imaging.BilinearInterpolationImageView;

private import mathematics.imaging.Image;


version (Tango)
{
	private import tango.math.Math;
	private import tango.io.Stdout;
}


//	Should be a view.



	/**
	* Bilinear interpolation, slow version.
	*/


class BilinearInterpolationImageView(T) : Image!(T)
{
private:
	Image!(T) _sourceImage;	
public:
	this( Image!(T) sourceImage )
	{
		assert(sourceImage !is null, "no source image");
		assert(sourceImage.getComponents() == 1, "source image must have only 1 component");
		_sourceImage = sourceImage;		
	}
	
	override T opIndex(int x, int y)
	{
		return _sourceImage[x,y];		
	}

	override T opIndex(int x, int y, int component)
	{
		assert( component == 0, "component");
		return _sourceImage[x,y,component];
	}

	override void opIndexAssign(T value, int x, int y)
	{
		_sourceImage[x,y] = value;
	}

	override void  opIndexAssign(T value, int x, int y, int component)
	{
		assert( component == 0, "component");
		_sourceImage[x,y,component] = value;
	}

	/**
	*	Gives access to a subpixel precision bilinear interpolation.
	*/	
	
	// http://en.wikipedia.org/wiki/Bilinear_interpolation
		
	T opIndex(double x, double y)
	{
		assert( x >= 0, "x>=0");
		assert( y >= 0, "y>=0");
		assert( x < _sourceImage.getWidth(), "width");
		assert( y < _sourceImage.getHeight(), "height");

		int x1 = rndint(floor(x));		//value between zero and one
		int y1 = rndint(floor(y));
		
		x = x - cast(double) x1;
		y = y - cast(double) y1;
		
		int x2 = x1+1;
		int y2 = y1+1;
		
		//Slows down a bit lot just for last column.		
		if (x2 >= _sourceImage.getWidth) x2 = _sourceImage.getWidth - 1;
		if (y2 >= _sourceImage.getHeight) y2 = _sourceImage.getHeight - 1;

		return _sourceImage[x1, y1]*(1.0-x)*(1.0-y)
					+_sourceImage[x2, y1]*x*(1.0-y)
					+_sourceImage[x1, y2]*(1.0-x)*y
					+_sourceImage[x2, y2]*x*y;
	}
	
	int getWidth()
	{
		return _sourceImage.getWidth();
	}
	int getHeight()
	{
		return _sourceImage.getHeight();
	}
	
	int getComponents()
	{
		return _sourceImage.getComponents();
	}
}


	/**
	* Bilinear interpolation, faster version.
	*/



class OutsideBilinearInterpolationImageView(T):Image!(T)
{
private:
	Image!(T) _sourceImage;
public:
	/**
	* The sourceImage must be accessible from the entire range even negative
	* coordinates. Thus going outside the source image.
	*/
	this(Image!(T) sourceImage )
	{
		assert(sourceImage !is null, "no source image");
		assert(sourceImage.getComponents() == 1, "source image must have only 1 component");
		_sourceImage = sourceImage;		
	}
	
	override T opIndex(int x, int y)
	{
		return _sourceImage[x,y];		
	}

	override T opIndex(int x, int y, int component)
	{
		assert( component == 0, "component");
		return _sourceImage[x,y,component];
	}

	override void opIndexAssign(T value, int x, int y)
	{
		assert( x >=0 && x < getWidth);
		assert( y >=0 && y < getWidth);
		_sourceImage[x,y] = value;
	}

	override void  opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >=0 && x < getWidth);
		assert( y >=0 && y < getWidth);
		assert( component == 0, "component");
		_sourceImage[x,y,component] = value;
	}

	/**
	*	Gives access to a subpixel precision bilinear interpolation.
	*/	
	
	// http://en.wikipedia.org/wiki/Bilinear_interpolation
		
	T opIndex(double x, double y)
	{
		int x1 = rndint(floor(x));		//value between zero and one
		int y1 = rndint(floor(y));
		
		x = x - cast(double) x1;
		y = y - cast(double) y1;
		
		int x2 = x1+1;
		int y2 = y1+1;
		
		return _sourceImage[x1, y1]*(1.0-x)*(1.0-y)
					+_sourceImage[x2, y1]*x*(1.0-y)
					+_sourceImage[x1, y2]*(1.0-x)*y
					+_sourceImage[x2, y2]*x*y;
	}
	int getWidth()
	{
		return _sourceImage.getWidth();
	}
	int getHeight()
	{
		return _sourceImage.getHeight();
	}
	
	int getComponents()
	{
		return _sourceImage.getComponents();
	}
	real getSumOfArea(double x1, double y1, double x2, double y2)
	{
		return this[x2,y2] + this[x1,y1] - this[x1,y2] - this[x2,y1];
	}
}



