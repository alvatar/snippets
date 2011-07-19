/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

module mathematics.numerical.Filters;


version (Tango)
{
	private import tango.math.Math;
}
else	//Phobos
{
	private	import std.math;
}


/*
	use mathextra.

real gauss( real mean, real variance, real t)
{
	return 1/sqrt(2*pi*variance*variance) * exp( (x-mean)^2 / (2*variance*variance));
}

real guassnormal( real variance, real t)
{
	return 1/sqrt(2*pi*variance*variance) * exp(x^2 / (2*variance*variance));
}
*/


T circularFilter(T)( T [] filter, int index)
{
	index=index%filter.length;
	return filter[index];
}

void correlation(T) ( T [] input, T [] filter, T [] output)
{
	T intermediate;
	for (int t = 0; t < input.length ; t++)
	{
		intermediate = 0;
		for (int x = 0; x < filter.length; x++)
			intermediate += input[t+x]*filter[x];
		output[t] = intermediate;
	}
}

void convolution(T) ( T [] input, T [] filter, T [] output)
{
	T intermediate;
	int l=filter.length-1;
	for (int t = 0; t < input.length ; t++)
	{
		intermediate = 0;
		for (int x = 0; x < filter.length; x++)
			intermediate += input[t+x]*filter[l-x];
		output[t] = intermediate;
	}
}









	








