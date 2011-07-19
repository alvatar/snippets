/**
 * TransformedImageView
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
 *
 *  Bugs: Not tested, not finished.
 */
 

module mathematics.imaging.TransformedImageView;

private import mathematics.imaging.Image;
private import mathematics.imaging.ImageView;
private import mathematics.numerical.Matrix;
private import mathematics.numerical.VectorFixed;

private import tango.math.Math;


	//
	// Provides a view of the image that is smaller than the original image.
	//

/*

class TransformedImageView(T, int COMPONENTS): ImageView!(T, COMPONENTS)
{
private:
	Matrix!(double) _matrix;
	Image!(T) _sourceImage;
	int _left;
	int _right;
	int _top;
	int _bottom;
public:

	this(Image!(T) sourceImage)
	{
		super(sourceImage.getWidth, sourceImage.getHeight);		//copies width and height from the source.
		_sourceImage = sourceImage;
		_matrix = new Matrix!(double)(3,3);		
		_matrix[0,0]=1.0;_matrix[0,1]=0.0;_matrix[0,2]=0.0;
		_matrix[1,0]=0.0;_matrix[1,1]=1.0;_matrix[1,2]=0.0;
		_matrix[2,0]=0.0;_matrix[2,1]=0.0;_matrix[2,2]=1.0;
	}

	//
	//	Provides a transformed view of the original image.
	//
	//	Params:
	//   sourceImage                  = The image source.
	//   width                        = Width of the current view.
	//   height                       = Height of the current view.
	//
	

	this(Image!(T) sourceImage, int width, int height)
	{	
		super(width, height);		//copies width and height from the source.
		_sourceImage = sourceImage;
		_matrix = new Matrix!(double)(3,3);		
		_matrix[0,0]=1.0;_matrix[0,1]=0.0;_matrix[0,2]=0.0;
		_matrix[1,0]=0.0;_matrix[1,1]=1.0;_matrix[1,2]=0.0;
		_matrix[2,0]=0.0;_matrix[2,1]=0.0;_matrix[2,2]=1.0;
		setLeftRight( 0, width-1);
		setTopBottom( 0, height-1);
	}
	
	
	//
	// Counterclockwise rotation and translation of the view.
	//
	//	The image y axis is pointed downwards
	//
	//	Params:
	//   x					=	The x coordinate of the top left point of the view in the source image coordinates.
	//  y         = The y coordinate of the top left point of the view in the source image coordinates.
	//  angle     = The angle in radians.
	//
	
	void setRotation( int x, int y, real angle)
	{		
		Matrix!(double) translation = new Matrix!(double)(3,3);
		translation[0,0]=1.0;translation[0,1]=0.0;translation[0,2]= x;
		translation[1,0]=0.0;translation[1,1]=1.0;translation[1,2]= y;
		translation[2,0]=0.0;translation[2,1]=0.0;translation[2,2]=1.0;
		double c = cos(angle);
		double s = sin(angle);
		// y axis pointing downwards. (if you are wondering why this looks strange)
		Matrix!(double) rotation = new Matrix!(double)(3,3);
		rotation[0,0]=c;	rotation[0,1]= s;		rotation[0,2]= 0;
		rotation[1,0]=s;	rotation[1,1]=-c;		rotation[1,2]= 0;
		rotation[2,0]=0.0;rotation[2,1]=0.0;rotation[2,2]=1.0;
		_matrix = translation * rotation; //Volunteers to make the calculation in advance? :-)
		if (!cornersWithinView()) throw new Exception("The view exceeds the dimensions of the original image.");
	}
	void setTransformation(Matrix!(double) matrix)
	{
		assert( matrix.getColumnCount() == 3);
		assert( matrix.getRowCount() == 3);
		_matrix = matrix;
		if (!cornersWithinView()) throw new Exception("The view exceeds the dimensions of the original image.");
	}
	override T opIndex(int x, int y)
	{
		VectorFixed!(double, cast(uint) 3) position = _matrix.mul3x3( VectorFixed!(double,cast(uint)3)(x,y,1.0));
		return _sourceImage[rndint(round(position[0])), rndint(round(position[1]))];
		//return super.opIndex();
	}
		
	override T opIndex(int x, int y, int component)
	{
		VectorFixed!(double, cast(uint) 3) position = _matrix.mul3x3(VectorFixed!(double,cast(uint)3)(x,y,1.0));
		return _sourceImage[rndint(round(position[0])), rndint(round(position[1])), component];
	}
	
	//maybe an error to call the super of a virtual function. Is a super calling
	//the last virtual function this is a design flaw.
	
	override void opIndexAssign(T value, int x, int y)
	{
		VectorFixed!(double,cast(uint) 3) position = _matrix.mul3x3(VectorFixed!(double,cast(uint)3)(x,y,1.0));
		_sourceImage[rndint(round(position[0])), rndint(round(position[1]))] = value;
	}
	
	override void opIndexAssign(T value, int x, int y, int component)
	{
		VectorFixed!(double, cast(uint)3) position = _matrix.mul3x3(VectorFixed!(double, cast(uint)3)(x,y,1.0));
		_sourceImage[rndint(round(position[0])), rndint(round(position[1])), component] = value;
	}
	
	
	void setLeftRight( int left, int right)
	{
		int width = right - left + 1;
		assert(width > 0);
		assert(width == getWidth());
		_left = left;
		_right = right;
	}
	
	void setTopBottom( int top, int bottom )
	{
			int height = bottom - top +1;
			assert(height > 0);
			assert(height == getHeight());
			_top = top;
			_bottom = bottom;
	}
	
	bool pointWithinView(int x, int y)
	{
		VectorFixed!(double,cast(uint) 3) position = _matrix.mul3x3(VectorFixed!(double,cast(uint)3)(x,y,1.0));
		return  position[0] >=0 && position[0] < _sourceImage.getWidth() && position[1] >=0 && position[1] < _sourceImage.getHeight();
	}
	
	//
	//  Check if the of the view are in the original source image.
	//
	
	bool cornersWithinView()
	{
		VectorFixed!(double,cast(uint) 3) position;
		bool result = true;
		position = _matrix.mul3x3(VectorFixed!(double, cast(uint) 3)(_left,_top,1.0));		
		result = result && position[0] >=0 && position[0] < _sourceImage.getWidth() && position[1] >=0 && position[1] < _sourceImage.getHeight();
		
		position = _matrix.mul3x3(VectorFixed!(double, cast(uint)3)(_right,_top,1.0));		
		result = result && position[0] >=0 && position[0] < _sourceImage.getWidth() && position[1] >=0 && position[1] < _sourceImage.getHeight();

		position = _matrix.mul3x3(VectorFixed!(double, cast(uint) 3)(_left,_bottom,1.0));		
		result = result && position[0] >=0 && position[0] < _sourceImage.getWidth() && position[1] >=0 && position[1] < _sourceImage.getHeight();

		position = _matrix.mul3x3(VectorFixed!(double,cast(uint) 3)(_right,_bottom,1.0));		
		result = result && position[0] >=0 && position[0] < _sourceImage.getWidth() && position[1] >=0 && position[1] < _sourceImage.getHeight();
		return result;
	}
}
	

alias TransformedImageView!(float, 1) testTransformedImageView;



*/


