/**********************************************************************************
 * Copyright (c) 2008-2009 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and/or associated documentation files (the
 * "Materials"), to deal in the Materials without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Materials, and to
 * permit persons to whom the Materials are furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Materials.
 *
 * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 **********************************************************************************/

// $Revision: 8407 $ on $Date: 2009-06-12 10:56:38 -0700 (Fri, 12 Jun 2009) $

module opencl.platform;

/* scalar types  */
alias byte		cl_char;
alias ubyte		cl_uchar;
alias short		cl_short;
alias ushort	cl_ushort;
alias int		cl_int;
alias uint		cl_uint;
alias long		cl_long;
alias ulong		cl_ulong;

alias ushort	cl_half;
alias float		cl_float;
alias double	cl_double;

/*
 * Vector types 
 *
 *  Note:   OpenCL requires that all types be naturally aligned. 
 *          This means that vector types must be naturally aligned.
 *          For example, a vector of four floats must be aligned to
 *          a 16 byte boundary (calculated as 4 * the natural 4-byte 
 *          alignment of the float).  The alignment qualifiers here
 *          will only function properly if your compiler supports them
 *          and if you don't actively work to defeat them.  For example,
 *          in order for a cl_float4 to be 16 byte aligned in a struct,
 *          the start of the struct must itself be 16-byte aligned. 
 *
 *          Maintaining proper alignment is the user's responsibility.
 */

alias byte[2]		cl_char2;
alias byte[4]		cl_char4;
alias byte[8]		cl_char8;
alias byte[16]		cl_char16;
alias ubyte[2]		cl_uchar2;
alias ubyte[4]		cl_uchar4;
alias ubyte[8]		cl_uchar8;
alias ubyte[16]		cl_uchar16;

alias short[2]		cl_short2;
alias short[4]		cl_short4;
alias short[8]		cl_short8;
alias short[16]		cl_short16;
alias ushort[2]		cl_ushort2;
alias ushort[4]		cl_ushort4;
alias ushort[8]		cl_ushort8;
alias ushort[16]	cl_ushort16;

alias int[2]		cl_int2;
alias int[4]		cl_int4;
alias int[8]		cl_int8;
alias int[16]		cl_int16;
alias uint[2]		cl_uint2;
alias uint[4]		cl_uint4;
alias uint[8]		cl_uint8;
alias uint[16]		cl_uint16;

alias long[2]		cl_long2;
alias long[4]		cl_long4;
alias long[8]		cl_long8;
alias long[16]		cl_long16;
alias ulong[2]		cl_ulong2;
alias ulong[4]		cl_ulong4;
alias ulong[8]		cl_ulong8;
alias ulong[16]		cl_ulong16;

alias float[2]		cl_float2;
alias float[4]		cl_float4;
alias float[8]		cl_float8;
alias float[16]		cl_float16;

alias double[2]		cl_double2;
alias double[4]		cl_double4;
alias double[8]		cl_double8;
alias double[16]	cl_double16;

/* There are no vector types for half */