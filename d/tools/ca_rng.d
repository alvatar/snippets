/*
Copyright (C) 2006  Tony Pasqualoni 

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

To acquire a copy of the GPLv2, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
Or, you know, just google.

The author can be contacted at oblivio_n at yahoo dot com.
*/

/*
 *  Tony Pasqualoni / Sept. 20, 2006
 *
 *  Cellular automaton random number generator
 *  Uses a 256-state automaton to generate random sequences of 32-bit unsigned integers.
 *
 *  Functions:
 *  ca_rng_initialize (unsigned int seed): initialize automaton using specified seed.
 *  unsigned int ca_rng_get_int (void): returns a 32-bit unsigned integer produced by the automaton.
 *
 */

// The original author is not responsible for this port; however, the license is unchanged.

module tools.ca_rng;
import tools.base;
const CA_WIDTH=2056;   // width in cells of cellular automaton
const RULESIZE=511;    // amount of elements in rule table

class Generator : IRandom {
  union {
    ubyte[CA_WIDTH] init_config;
    uint[CA_WIDTH/4] init_config_ints;
  }
  uint first_cell, last_cell, current;
  const ubyte[] rules = [
    100,75,16,3,229,51,197,118,24,62,198,11,141,152,241,188,2,17,71,47,179,177,126,231,202,243,59,25,77,196,30,134,
    199,163,34,216,21,84,37,182,224,186,64,79,225,45,143,20,48,147,209,221,125,29,99,12,46,190,102,220,80,215,242,
    105,15,53,0,67,68,69,70,89,109,195,170,78,210,131,42,110,181,145,40,114,254,85,107,87,72,192,90,201,162,122,86,
    252,94,129,98,132,193,249,156,172,219,230,153,54,180,151,83,214,123,88,164,167,116,117,7,27,23,213,235,5,65,124,
    60,127,236,149,44,28,58,121,191,13,250,10,232,112,101,217,183,239,8,32,228,174,49,113,247,158,106,218,154,66,
    226,157,50,26,253,93,205,41,133,165,61,161,187,169,6,171,81,248,56,175,246,36,178,52,57,212,39,176,184,185,245,
    63,35,189,206,76,104,233,194,19,43,159,108,55,200,155,14,74,244,255,222,207,208,137,128,135,96,144,18,95,234,
    139,173,92,1,203,115,223,130,97,91,227,146,4,31,120,211,38,22,138,140,237,238,251,240,160,142,119,73,103,166,33,
    148,9,111,136,168,150,82,204,100,75,16,3,229,51,197,118,24,62,198,11,141,152,241,188,2,17,71,47,179,177,126,231,
    202,243,59,25,77,196,30,134,199,163,34,216,21,84,37,182,224,186,64,79,225,45,143,20,48,147,209,221,125,29,99,12,
    46,190,102,220,80,215,242,105,15,53,0,67,68,69,70,89,109,195,170,78,210,131,42,110,181,145,40,114,254,85,107,87,
    72,192,90,201,162,122,86,252,94,129,98,132,193,249,156,172,219,230,153,54,180,151,83,214,123,88,164,167,116,117,
    7,27,23,213,235,5,65,124,60,127,236,149,44,28,58,121,191,13,250,10,232,112,101,217,183,239,8,32,228,174,49,113,
    247,158,106,218,154,66,226,157,50,26,253,93,205,41,133,165,61,161,187,169,6,171,81,248,56,175,246,36,178,52,57,
    212,39,176,184,185,245,63,35,189,206,76,104,233,194,19,43,159,108,55,200,155,14,74,244,255,222,207,208,137,128,
    135,96,144,18,95,234,139,173,92,1,203,115,223,130,97,91,227,146,4,31,120,211,38,22,138,140,237,238,251,240,160,
    142,119,73,103,166,33,148,9,111,136,168,150,82];
  void seed(uint s) {
    foreach (ref v; init_config) v = 0;
    init_config_ints[$-1] = s;
    if (s < uint.max) ++s;
    foreach (i, ref v; init_config) v = cast(ubyte) (s >> (i%32));
    last_cell = init_config.length; current = 0;
    for (int i = 0; i < CA_WIDTH * CA_WIDTH / 4; ++i) rand();
  }
  this(uint s) { seed(s); }
  uint prev(uint pos) { if (!pos) return init_config.length-1; else return pos - 1; }
  import tools.base;
  uint rand() {
    for (int i = 0; i < 4; ++i) {
      auto p = prev(current);
      init_config[current] = rules[init_config[p] + init_config[current]];
      current = p;
    }
    assert(current <= init_config.length-4);
    assert(current%4 == 0, Format("Invalid current: ", current));
    return init_config_ints[current/4];
  }
  alias rand opCall;
}

private Generator deflt;
void seed(uint seed) { deflt = new Generator(seed); }
uint rand() { if (!deflt) seed(23); return deflt(); }

float randf() { if (!deflt) seed(23); return 1f * deflt() / (1f * typeof(deflt()).max); }
