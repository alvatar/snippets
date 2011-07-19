/*
 * Small jpeg decoder library (Internal header)
 *
 * Copyright (c) 2006, Luc Saillard <luc@saillard.org>
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 *  this list of conditions and the following disclaimer in the documentation
 *  and/or other materials provided with the distribution.
 *
 * - Neither the name of the author nor the names of its contributors may be
 *  used to endorse or promote products derived from this software without
 *  specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
 * Ported to the D programming language by
 * Tomas Lindquist Olsen <tomas@famolsen.dk>
 */

module tinyjpeg.internal;

//#include <setjmp.h>

version(Tango)
    import tango.stdc.stdint;
else
    import std.stdint;

// this enables a lot of error checking, set to 0 to disable
enum { SANITY_CHECK = 1 }

enum
{
    HUFFMAN_BITS_SIZE = 256,
    HUFFMAN_HASH_NBITS = 9,
    HUFFMAN_HASH_SIZE = (1U<<HUFFMAN_HASH_NBITS),
    HUFFMAN_HASH_MASK = (HUFFMAN_HASH_SIZE-1),

    HUFFMAN_TABLES = 4,
    COMPONENTS = 3,
    JPEG_MAX_WIDTH = 2048,
    JPEG_MAX_HEIGHT = 2048
}

struct huffman_table
{
  /* Fast look up table, using HUFFMAN_HASH_NBITS bits we can have directly the symbol,
   * if the symbol is <0, then we need to look into the tree table */
  short lookup[HUFFMAN_HASH_SIZE];
  /* code size: give the number of bits of a symbol is encoded */
  ubyte code_size[HUFFMAN_HASH_SIZE];
  /* some place to store value that is not encoded in the lookup table
   * FIXME: Calculate if 256 value is enough to store all values
   */
  uint16_t slowtable[16-HUFFMAN_HASH_NBITS][256];
}

struct component
{
  uint Hfactor;
  uint Vfactor;
  float *Q_table;       /* Pointer to the quantisation table to use */
  huffman_table *AC_table;
  huffman_table *DC_table;
  short previous_DC;    /* Previous DC coefficient */
  short DCT[64];        /* DCT coef */
  static if (SANITY_CHECK)
  {
    uint cid;
  }
};


typedef void (*decode_MCU_fct) (jdec_private *priv);
typedef void (*convert_colorspace_fct) (jdec_private *priv);

struct jdec_private
{
  /* Public variables */
  uint8_t *components[COMPONENTS];
  uint width, height;   /* Size of the image */
  uint flags;

  /* Private variables */
  const ubyte* stream_begin, stream_end;
  uint stream_length;

  const ubyte *stream;  /* Pointer to the current stream */
  uint reservoir, nbits_in_reservoir;

  component component_infos[COMPONENTS];
  float Q_tables[COMPONENTS][64];       /* quantization tables */
  huffman_table HTDC[HUFFMAN_TABLES];    /* DC huffman tables   */
  huffman_table HTAC[HUFFMAN_TABLES];    /* AC huffman tables   */
  int default_huffman_table_initialized;
  int restart_interval;
  int restarts_to_go;               /* MCUs left in this restart interval */
  int last_rst_marker_seen;         /* Rst marker is incremented each time */

  /* Temp space used after the IDCT to store each components */
  uint8_t Y[64*4];
  uint8_t[64] Cr, Cb;

  //jmp_buf jump_state;

  /* Internal Pointer use for colorspace conversion, do not modify it !!! */
  uint8_t *plane[COMPONENTS];

  // error string
  char[] error_string;
};
