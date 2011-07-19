/**
 * Color conversion routines.
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


module mathematics.imaging.Color;



/*
struct ColorFixed!(T, uint COMPONENTS)
{
private:
	T [COMPONENTS] _data;
public:
	T opIndex(int i)
	{
		assert( i >= 0 && i < _data.length, "Index out of range");
		return _data[i];
	}
	void opIndexAssign(T value, int i)
	{
		assert( i >= 0 && i < _data.length, "Index out of range");
		assert( value >= 0 && value <= 1, "Value out of range");
		_data[i] = value;
	}
}
*/


template convertFloatingColor(T)
{
	T greyFromRGB( T red, T green, T blue)
	{
		assert( red >= 0 && red <= 1.0, "Color red not in range");
		assert( green >= 0 && green <= 1.0, "Color green not in range");
		assert( blue >= 0 && blue <= 1.0, "Color blue not in range");
		return (red* 0.33333 + green*0.333335 + blue*0.333335);
	}
	
	byte toByte(T colorValue)
	{
		assert( 0 >= colorValue && colorValue <= 1.0, "colorValue out of range");
		return cast(byte) colorValue * 255;
	}

	T toFloating(byte colorValue)
	{
		assert( 0 >= colorValue && colorValue <= 255, "colorValue out of range");
		return (cast(T) colorValue)/255.0;
	}
}
	

ubyte greyFromRGB( ubyte red, ubyte green, ubyte blue)
{
	return cast(byte) (red*85.0/255.0 + green * 85.0/255.0 + blue*85.0/255.0);
}




	
/*
	GL_COLOR_INDEX
	GL_RGB,	GL_RGBA
	GL_BGR,	GL_BGRA
	GL_RED,	GL_GREEN, GL_BLUE
	GL_ALPHA, GL_LUMINANCE,	GL_LUMINANCE£_ALPHA
	GL_STENCEL_INDEX, GL_DEPTH_COMPONENT

	GL_UNSIGNED_BYTE_3_3_2		8 bit	= ubyte in D
	GL_UNSIGNED_BYTE_2_3_3_REV

	GL_UNSIGNED_SHORT_5_6_5		16 bit
	GL_UNSIGNED_SHORT_5_6_5_REV

	GL_UNSIGNED_SHORT_4_4_4_4	16 bit	
*/
	




		






       



	
