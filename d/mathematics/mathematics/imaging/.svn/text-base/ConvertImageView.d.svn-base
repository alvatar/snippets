/**
 * Conversion between different image formats.
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




module mathematics.imaging.ConvertImageView;

private { 
	import mathematics.imaging.Image;
	import mathematics.imaging.ImageView;
	import mathematics.imaging.Color;
	import tango.io.Stdout;
}


//	Blah depricated.


//
// Instead of walking through the RGB via the x coordinate
// you have to walk through them via the component coordinate.
//


/*

class ImageViewComponents3From1(T): ImageView!(T, 3)
{
protected:
	Image!(T) _sourceImage;
public:
	this (Image!(T) sourceImage)
	{
		assert(sourceImage !is null, "no source image");
		assert(sourceImage.getComponents() == 1, "source image must have only 1 component");
		assert(sourceImage.getWidth() % 3 == 0, "The source image has an incorrect width");
		super(sourceImage.getWidth()/3, sourceImage.getHeight());
		_sourceImage = sourceImage;
	}
	
	override T opIndex(int x, int y)
	{
		assert( x >= 0, "x >= 0");
		assert( y >= 0, "y >= 0");

		assert( x < getWidth(), "x >= 0");
		assert( y < getHeight(), "y >= 0");

		return _sourceImage[x * getComponents(), y];		
	}

	override void opIndexAssign(T value, int x, int y)
	{
		assert( x >= 0, "x >= 0");
		assert( y >= 0, "y >= 0");

		assert( x < getWidth(), "x >= 0");
		assert( y < getHeight(), "y >= 0");

		_sourceImage[x * getComponents(), y] = value;
	}

	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0, "x >= 0");
		assert( y >= 0, "y >= 0");
		assert( component >= 0, "component >= 0");
		assert( x < getWidth(), "x >= 0");
		assert( y < getHeight(), "y >= 0");
		assert( component < getComponents(), "component >= 0");

		return _sourceImage[x * getComponents() + component, y];
	}

	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0, "x >= 0");
		assert( y >= 0, "y >= 0");
		assert( component >= 0, "component >= 0");
		assert( x < getWidth(), "x >= 0");
		assert( y < getHeight(), "y >= 0");
		assert( component < getComponents(), "component >= 0");

		_sourceImage[x * getComponents() + component, y] = value;
	}
}




//
// Displays a black and white image from a RGB image
// of the same type.
//


class ImageViewFloatingGreyFromRGB(T):ImageView!(T, 1)
{
protected:
	Image!(T) _sourceImage;
public:
	this(Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == 3, "the source image must have 3 components");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;
	}

	override T opIndex(int x, int y)
	{
		return convertFloatingColor!(T).greyFromRGB(_sourceImage[x,y,0], _sourceImage[x,y,1], _sourceImage[x,y,2]);
	}

	override void opIndexAssign(T value, int x, int y)
	{
		_sourceImage[x, y, 0] = value;
		_sourceImage[x, y, 1] = value;
		_sourceImage[x, y, 2] = value;
	}

	override T opIndex(int x, int y, int component)
	{
		assert( component == 1, "index out of bound");
		return this.opIndex(x,y);
	}

	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( component == 1, "index out of bound");
		assert( value >= 0 && value <= 1, "value out of range");
		this.opIndexAssign( value, x, y);
	}
}

//
// Produces a black and white image from an RGB image.
// operates on bytes only. (TODO: Merge)
//


class ImageViewUByteGreyFromRGB: ImageView!(ubyte, 1)
{
protected:
	Image!(ubyte) _sourceImage;
public:
	this(Image!(ubyte) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == 3, "the source image must have 3 components");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;	
	}

	override ubyte opIndex(int x, int y)
	{
		return greyFromRGB( _sourceImage[x,y,0], _sourceImage[x,y,1], _sourceImage[x,y,2]);
	}

	override void opIndexAssign(ubyte value, int x, int y)
	{
		_sourceImage[x, y, 0] = value;
		_sourceImage[x, y, 1] = value;
		_sourceImage[x, y, 2] = value;
	}

	override ubyte opIndex(int x, int y, int component)
	{
		assert( component == 1, "index out of bound");
		return this.opIndex(x,y);
	}

	override void opIndexAssign(ubyte value, int x, int y, int component)
	{
		assert( component == 1, "index out of bound");
		assert( value >= 0 && value <= 1, "value out of range");
		this.opIndexAssign( value, x, y);
	}
}



//
// A RGB image from a grey image.
//

class ImageViewFloatingRGBFromGrey(T): ImageView!(T,3)
{
protected:
	Image!(T) _sourceImage;
public:
	this(Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == 1, "the source image must have 1 components");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;
	}

	override T opIndex(int x, int y)
	{
		return convertFloatingColor!(T).greyFromRGB(_sourceImage[x,y,0], _sourceImage[x,y,1], _sourceImage[x,y,2]);
	}

	override void opIndexAssign(T value, int x, int y)
	{
		_sourceImage[x, y, 0] = value;
		_sourceImage[x, y, 1] = value;
		_sourceImage[x, y, 2] = value;
	}

	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");

		return _sourceImage[x,y];
	}
	

	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");

		assert( value >= 0 && value <= 1.0, "value out of range");

		//TODO: add color weights

		T red = _sourceImage[x,y];
		T green = _sourceImage[x,y];
		T blue = _sourceImage[x,y];

		T newColor;
		if (component == 0)
		{
			newColor = convertFloatingColor!(T).greyFromRGB(value, green, blue);
		} else if (component == 1)
		{
			newColor = convertFloatingColor!(T).greyFromRGB(red, value, blue);
		}
		else //components == 2
		{
			newColor = convertFloatingColor!(T).greyFromRGB( red, green, value);
		}
		_sourceImage[x,y] = newColor;
		
	}
}

//
//	Provides a float view of a UByte
//


class ImageViewFloatingFromUByte(T): ImageView!(T, 1)
{
protected:
	Image!(ubyte) _sourceImage;
public:
	this(Image!(ubyte) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == 1, "the source image must have 1 components");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;
	}

	override T opIndex(int x, int y)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert(_sourceImage[x,y,0] >=0 && _sourceImage[x,y,0]<=255, "value of image not within range (0 -> 255)");		
		return cast(T) (_sourceImage[x,y,0]/255.0);		//return floating from byte source image.
	}

	override void opIndexAssign(T value, int x, int y)		//floating view
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");		
		assert( value >= 0 && value <= 1.0, 
				"value not within range (0 -> 1.0), possible cause: you can not pass negative values to a float view");
		_sourceImage[x,y,0] = cast(ubyte) (value*255);		//byte source image
	}

	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		return this[x,y];
	}	

	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		this[x,y] = value;
	}



}


//
//	Provides a UByte view of a floating view.
//




class ImageViewUByteFromFloating(T):ImageView!(ubyte, 1)
{
protected:
	Image!(T) _sourceImage;
public:


	this(Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == 1, "the source image must have 1 components");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;
	}


	override ubyte opIndex(int x, int y)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");

		assert(_sourceImage[x,y,0] >=0 && _sourceImage[x,y,0]<=1.0, "value of image not within range (0 -> 1.0)");		
		return cast(ubyte) (255*_sourceImage[x,y,0]);		//return byte from floating source
	}

	override void opIndexAssign(ubyte value, int x, int y)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( value >= 0 && value <= 255, "value of image not within range (0 -> 255)");
		_sourceImage[x,y,0] = cast(T) value / 255.0;		//floating source
	}

	override ubyte opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");

		return this[x,y];
	}	

	override void opIndexAssign(ubyte value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		this[x,y] = value;
	}
}




//
// A view with 4 components from a view with 3 components.
// the latest component gets the value zero assigned.
// You can not set the last component.
//


class ImageViewABCDFromABC(T):ImageView!(T,4)
{
protected:
	Image!(T) _sourceImage;
public:
	this(Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == 3, "the source image must have 3 components");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;
	}

	override T opIndex(int x, int y)
	{
		return this.opIndex(x,y,0);
	}

	override void opIndexAssign(T value, int x, int y)
	{
		this[x,y,0] = value;
	}

	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");

		return _sourceImage[x,y,component];
	}	

	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");

		assert( component == 4, "can not set the fourth component");

		_sourceImage[x,y,component] = value;
	}
}


//
// Creates a view with only the 3 first components from
// an image with 4 components.
//

class ImageViewABCFromABCD(T):ImageView!(T,3)
{
protected:
	Image!(T) _sourceImage;
public:
	this(Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == 4, "the source image must have 4 components");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;
	}

	override T opIndex(int x, int y)
	{
		return this.opIndex(x,y,0);
	}

	override void opIndexAssign(T value, int x, int y)
	{
		this[x,y,0] = value;
	}

	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");

		return _sourceImage[x,y,component];
	}	

	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");

		_sourceImage[x,y,component] = value;
	}
}

//
//	Just provides and Image View from any Image.
//

class ImageViewFromImage(T, int COMPONENTS):ImageView!(T, COMPONENTS)
{
protected:
	Image!(T) _sourceImage;
public:
	this( Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == COMPONENTS, "the source image and view, components don't match");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;		
	}
	override T opIndex(int x, int y)
	{
		return this.opIndex(x,y,0);
	}

	override void opIndexAssign(T value, int x, int y)
	{
		this[x,y,0] = value;
	}
	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		return _sourceImage[x,y,component];
	}	
	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		_sourceImage[x,y,component] = value;
	}
}


//
//	Provides a Mirror Y view.
//



class ImageViewMirrorY(T, int COMPONENTS):ImageView!(T, COMPONENTS)
{
protected:
	Image!(T) _sourceImage;
public:
	this( Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == COMPONENTS, "the source image and view, components don't match");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;		
	}
	override T opIndex(int x, int y)
	{
		return this.opIndex(x,y,0);
	}

	override void opIndexAssign(T value, int x, int y)
	{
		this[x,y,0] = value;
	}
	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		return _sourceImage[x,this.getHeight()-1-y,component];
	}	
	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		_sourceImage[x,this.getHeight()-1-y,component] = value;
	}
}

//
//	Provides a Mirror X view
//

class ImageViewMirrorX(T, int COMPONENTS):ImageView!(T, COMPONENTS)
{
protected:
	Image!(T) _sourceImage;
public:
	this( Image!(T) sourceImage)
	{
		assert( sourceImage !is null, "no source image");
		assert( sourceImage.getComponents() == COMPONENTS, "the source image and view, components don't match");
		super( sourceImage.getWidth(), sourceImage.getHeight());
		_sourceImage = sourceImage;		
	}
	override T opIndex(int x, int y)
	{
		return this.opIndex(x,y,0);
	}

	override void opIndexAssign(T value, int x, int y)
	{
		this[x,y,0] = value;
	}
	override T opIndex(int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		return _sourceImage[this.getWidth()-1-x,y,component];
	}	
	override void opIndexAssign(T value, int x, int y, int component)
	{
		assert( x >= 0 && x < getWidth(), "index x out of bound");
		assert( y >=0 && y < getHeight(), "index y out of bound");
		assert( component >= 0 && component < getComponents(), "index component out of bound");
		_sourceImage[this.getWidth()-1-x,y,component] = value;
	}
}



*/






