/**
 * Integral Image.
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



module mathematics.imaging.algorithms.IntegralImage;

private import mathematics.imaging.Image;
private import mathematics.imaging.MemoryImage;



class IntegralImage(T):MemoryImage!(T)
{
public:
	this( Image!(T) sourceImage)
	{
		assert(sourceImage.getComponents == 1);
		assert(sourceImage.getWidth > 0);
		assert(sourceImage.getHeight > 0);
		super(sourceImage.getWidth, sourceImage.getHeight, sourceImage.getComponents);
		this[0,0] = sourceImage[0,0];
		for (int x = 1; x < getWidth; x++)
			this[x, 0] = super.opIndex(x-1, 0) + sourceImage[x,0];
		for (int y = 1; y < getHeight; y++)
			this[0, y] = super.opIndex(0, y-1) + sourceImage[0,y];
		for (int x = 1; x < getWidth; x++)
			for (int y = 1; y < getHeight; y++)
				this[x,y] = super.opIndex(x-1, y) + super.opIndex(x,y-1) - super.opIndex(x-1,y-1) + sourceImage[x,y];
	}
	
	
	
	/**
	* Returns the value of the integral image.
	*
	* The coordinates may exceed the width and height of the image.
	*/
	
	override T opIndex(int x, int y)
	{
		if (x < 0 || y < 0)
			return 0;
		if (x >= this.getWidth)
			x = this.getWidth - 1;
		if (y >= this.getHeight)
			y = this.getHeight - 1;
		return  super.opIndex(x,y);	
	}
	
	/**
	* Returns the value of the integral image.
	*
	* The coordinates (except component) may exceed the width and height of the image.
	*/

	
	override T opIndex(int x, int y, int component)
	{
		assert( component == 0);
		return this.opIndex(x,y);
	}
	/**
	* Finds the sum of an area of the original sourceImage.
	* The coordinates may exceed the width and height of the image.
	*/
	
	
	T getSumOfArea(int x1, int y1, int x2, int y2)
	{
		return this[x2,y2] + this[x1,y1] - this[x1,y2] - this[x2,y1];
	}
	
	/*	
	T getSumOfArea( int x, int y, int width, int height)
	{
		return this[x, y] - this[[x, y-height] - this[x-width, y]+ this[x-width, y-height];
	}
	*/
	
	
	
	/*
	T getSumOfArea( Vector!(int,2) p1, Vector!(int,2) p2)
	{
		return this[p2[0],p2[1]] + this[p1[0], p1[1]] - this[p1[0],p2[1]] - this[p2[0],p1[1]] ;	
	}
	*/
}

//alias IntegralImage!(float) testIntegralImage;













