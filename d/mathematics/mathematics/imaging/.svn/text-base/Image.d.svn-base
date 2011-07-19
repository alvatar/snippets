/**
 * Image interface in 3 dimensions.
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


module mathematics.imaging.Image;


interface ImageView2(T)
{
	T opIndex(int x, int y);
	void opIndexAssign(T value, int x, int y);
	int left();
	int right();
	int top();
	int bottom();	
}

interface ImageView3(T)
{
	void opIndexAssign(T value,  int x, int y, int component);
	T opIndex(int x, int y, int component);
	int left();
	int right();
	int top();
	int bottom();	
}


/**
* This is the abstract base class to access the elements of
* an image. You can not instantiate this.
*/


//TODO: Depricated this?


class ImageBase(T)
{
protected:
//	int _imageWidth;
//	int _imageHeight;
//	int _imageComponents;

/*
	this(int width, int height, int components)
	{
		assert( width > 0, "width > 0 failed");
		assert( height > 0, "height > 0 failed");
		assert( components > 0, "components > 0 failed");
		_imageWidth = width;
		_imageHeight = height;
		_imageComponents = components;
	}
*/

public:
	/**
	* Gives access to the components of an image.
	*
	* PARAMS:
	*	x		=	the x coordinate (In some cases can be negative)
	*	y		=	the y coordinate (In some cases can be negative)
	*	components	=	what comoponent
	*/

//	abstract T opIndex(int x, int y);
	
	/**
	* Gives access to the components of an image.
	*
	* PARAMS:
	*	x		=	the x coordinate
	*	y		=	the y coordinate
	*	components	=	what comoponent
	*/

//	abstract void opIndexAssign( T value, int x, int y);

	/**
	* Gives access to the components of an image.
	*
	* PARAMS:
	*	x		=	the x coordinate
	*	y		=	the y coordinate
	*	components	=	what comoponent
	*/

	abstract T opIndex( uint x, uint y, uint component);

	/**
	* Sets a certain component.
	*
	* PARAMS:
	*	x		=	the x coordinate
	*	y		=	the y coordinate
	*	components	=	what comoponent
	*/
	abstract void opIndexAssign(T value,  uint x, uint y, uint component);


	/**
	* Retrieve the width of the image.
	*
	* RETURNS: the width of the image.
	*/

	abstract uint getWidth();
	
	/**
	* Retrieve the height of the image.
	*
	* RETURNS: the height of the image.
	*/

	abstract uint getHeight();


	/**
	* Returns the number of components of the image. An RGB image would have
	* 3 components.
	*/

	abstract uint getComponents();
}










/*
Large image small image, what to do? Aargh.


class LargeImage(T)
{
	abstract T opIndex(uint plane, uint x, uint y, uint component);
	abstract void opIndexAssign(T value, uint plane, uint x, uint y, uint component);
	abstract uint getWidth();
	abstract uint getHeight();
	abstract uint getComponents();
	abstract uint getPlanes();
}
*/

