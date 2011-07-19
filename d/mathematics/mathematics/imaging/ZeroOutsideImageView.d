/**
 * ZeroOutsideImageView
 * 
 * Copyright: 	Copyright (C) 2006-2007 Peter Van Isacker
 * Authors: 	Peter Van Isacker (sclytrack)
 * Date:		September 18, 2007
 * License:	Copyright (C) 2006-2007 Peter Van Isacker
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 *	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *	version:	initial
 */
 
 
module mathematics.imaging.ZeroOutsideImageView;

private import mathematics.imaging.Image;
private import mathematics.imaging.ImageView;

	/**
	*	All values outside the images displays as zero.
	*/

class ZeroOutsideImageView(T):Image!(T)
{
private:
	Image!(T) _sourceImage;
public:
	this(Image!(T) sourceImage)
	{
		_sourceImage = sourceImage;
	}
	override T opIndex(int x, int y)
	{
		if (x < 0) return 0;
		if (y < 0) return 0;
		if (x >= getWidth()) return 0;
		if (y >= getHeight()) return 0;
		return _sourceImage[x,y];
	}
	override T opIndex(int x, int y, int component)
	{
		if (x < 0) return 0;
		if (y < 0) return 0;
		if (component < 0) return 0;
		if (x >= getWidth()) return 0;
		if (y >= getHeight()) return 0;
		if (component >= getComponents()) return 0;
		return _sourceImage[x,y];		
	}	
	override void opIndexAssign( T value, int x, int y)
	{
		assert( x >= 0 && x < getWidth);
		assert( y >= 0 && y < getHeight);
		_sourceImage[x,y] = value;
	}
	override void opIndexAssign( T value, int x, int y,int component)
	{
		assert( x >= 0 && x < getWidth);
		assert( y >= 0 && y < getHeight);
		assert( component >= 0 && component < getComponents);		
		_sourceImage[x,y,component] = value;
	}
	override int getWidth()
	{
			return _sourceImage.getWidth;
	}
	override int getHeight()
	{
		return _sourceImage.getHeight;
	}
	override int getComponents()
	{
		return _sourceImage.getComponents;
	}
}
