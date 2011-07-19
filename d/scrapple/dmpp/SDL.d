module SDL;

extern (C) :
struct SDL_ActiveEvent { ubyte type, gain, state; }
struct SDL_KeyboardEvent { ubyte type, which, state; SDL_keysym keysym; }
struct SDL_MouseMotionEvent { ubyte type, which, state; ushort x, y; short xrel, yrel; }
struct SDL_MouseButtonEvent { ubyte type, which, button, state; ushort x, y; }
struct SDL_JoyAxisEvent { ubyte type, which, axis; short value; }
struct SDL_JoyBallEvent { ubyte type, which, ball; short xrel, yrel; }
struct SDL_JoyHatEvent { ubyte type, which, hat, value; }
struct SDL_JoyButtonEvent { ubyte type, which, button, state; }
struct SDL_ResizeEvent { ubyte type; int w, h; }
struct SDL_ExposeEvent { ubyte type; }
struct SDL_QuitEvent { ubyte type; }
struct SDL_UserEvent { ubyte type; int code; void *data1, data2; }
struct SDL_SysWMEvent { ubyte type; void *msg; }

struct SDL_Color { ubyte r,g,b,unused;}
struct SDL_Palette { int ncolors; SDL_Color *colors; }
struct SDL_PixelFormat { SDL_Palette *palette; ubyte BitsPerPixel, BytesPerPixel, Rloss, Gloss, Bloss, Aloss,
	Rshift, Gshift, Bshift, Ashift; uint Rmask, Gmask, Bmask, Amask, colorkey; ubyte alpha;
}
struct SDL_Surface { uint flags; SDL_PixelFormat *format; int w, h; ushort pitch; void *pixels; int offset;
	void *hwdata; SDL_Rect clip_rect; uint unused; uint locked; void *map; uint format_version; int refcount;
}
struct SDL_Rect { short x, y; ushort w, h; }
struct SDL_VideoInfo { uint hw_available, wm_available, UnusedBits1, UnusedBits2, blit_hw, blit_hw_CC, blit_hw_A,
	blit_sw, blit_sw_CC, blit_sw_A, blit_fill, UnusedBits3, video_mem; SDL_PixelFormat *vfmt; int current_w, current_h; }

union SDL_Event {
	ubyte type;
	SDL_ActiveEvent active;
	SDL_KeyboardEvent key;
	SDL_MouseMotionEvent motion;
	SDL_MouseButtonEvent button;
	SDL_JoyAxisEvent jaxis;
	SDL_JoyBallEvent jball;
	SDL_JoyHatEvent jhat;
	SDL_JoyButtonEvent jbutton;
	SDL_ResizeEvent resize;
	SDL_ExposeEvent expose;
	SDL_QuitEvent quit;
	SDL_UserEvent user;
	SDL_SysWMEvent syswm;
}

enum SDL_EventType {
       SDL_NOEVENT = 0,			/* Unused (do not remove) */
       SDL_ACTIVEEVENT,			/* Application loses/gains visibility */
       SDL_KEYDOWN,			/* Keys pressed */
       SDL_KEYUP,			/* Keys released */
       SDL_MOUSEMOTION,			/* Mouse moved */
       SDL_MOUSEBUTTONDOWN,		/* Mouse button pressed */
       SDL_MOUSEBUTTONUP,		/* Mouse button released */
       SDL_JOYAXISMOTION,		/* Joystick axis motion */
       SDL_JOYBALLMOTION,		/* Joystick trackball motion */
       SDL_JOYHATMOTION,		/* Joystick hat position change */
       SDL_JOYBUTTONDOWN,		/* Joystick button pressed */
       SDL_JOYBUTTONUP,			/* Joystick button released */
       SDL_QUIT,			/* User-requested quit */
       SDL_SYSWMEVENT,			/* System specific event */
       SDL_EVENT_RESERVEDA,		/* Reserved for future use.. */
       SDL_EVENT_RESERVEDB,		/* Reserved for future use.. */
       SDL_VIDEORESIZE,			/* User resized video mode */
       SDL_VIDEOEXPOSE,			/* Screen needs to be redrawn */
       SDL_EVENT_RESERVED2,		/* Reserved for future use.. */
       SDL_EVENT_RESERVED3,		/* Reserved for future use.. */
       SDL_EVENT_RESERVED4,		/* Reserved for future use.. */
       SDL_EVENT_RESERVED5,		/* Reserved for future use.. */
       SDL_EVENT_RESERVED6,		/* Reserved for future use.. */
       SDL_EVENT_RESERVED7,		/* Reserved for future use.. */
       /* Events SDL_USEREVENT through SDL_MAXEVENTS-1 are for your use */
       SDL_USEREVENT = 24,
       /* This last event is only for bounding internal arrays
	  It is the number of bits in the event mask datatype -- Uint32
        */
       SDL_NUMEVENTS = 32
};

enum SDLKey {
	/* The keyboard syms have been cleverly chosen to map to ASCII */
	SDLK_UNKNOWN		= 0,
	SDLK_FIRST		= 0,
	SDLK_BACKSPACE		= 8,
	SDLK_TAB		= 9,
	SDLK_CLEAR		= 12,
	SDLK_RETURN		= 13,
	SDLK_PAUSE		= 19,
	SDLK_ESCAPE		= 27,
	SDLK_SPACE		= 32,
	SDLK_EXCLAIM		= 33,
	SDLK_QUOTEDBL		= 34,
	SDLK_HASH		= 35,
	SDLK_DOLLAR		= 36,
	SDLK_AMPERSAND		= 38,
	SDLK_QUOTE		= 39,
	SDLK_LEFTPAREN		= 40,
	SDLK_RIGHTPAREN		= 41,
	SDLK_ASTERISK		= 42,
	SDLK_PLUS		= 43,
	SDLK_COMMA		= 44,
	SDLK_MINUS		= 45,
	SDLK_PERIOD		= 46,
	SDLK_SLASH		= 47,
	SDLK_0			= 48,
	SDLK_1			= 49,
	SDLK_2			= 50,
	SDLK_3			= 51,
	SDLK_4			= 52,
	SDLK_5			= 53,
	SDLK_6			= 54,
	SDLK_7			= 55,
	SDLK_8			= 56,
	SDLK_9			= 57,
	SDLK_COLON		= 58,
	SDLK_SEMICOLON		= 59,
	SDLK_LESS		= 60,
	SDLK_EQUALS		= 61,
	SDLK_GREATER		= 62,
	SDLK_QUESTION		= 63,
	SDLK_AT			= 64,
	/*
	   Skip uppercase letters
	 */
	SDLK_LEFTBRACKET	= 91,
	SDLK_BACKSLASH		= 92,
	SDLK_RIGHTBRACKET	= 93,
	SDLK_CARET		= 94,
	SDLK_UNDERSCORE		= 95,
	SDLK_BACKQUOTE		= 96,
	SDLK_a			= 97,
	SDLK_b			= 98,
	SDLK_c			= 99,
	SDLK_d			= 100,
	SDLK_e			= 101,
	SDLK_f			= 102,
	SDLK_g			= 103,
	SDLK_h			= 104,
	SDLK_i			= 105,
	SDLK_j			= 106,
	SDLK_k			= 107,
	SDLK_l			= 108,
	SDLK_m			= 109,
	SDLK_n			= 110,
	SDLK_o			= 111,
	SDLK_p			= 112,
	SDLK_q			= 113,
	SDLK_r			= 114,
	SDLK_s			= 115,
	SDLK_t			= 116,
	SDLK_u			= 117,
	SDLK_v			= 118,
	SDLK_w			= 119,
	SDLK_x			= 120,
	SDLK_y			= 121,
	SDLK_z			= 122,
	SDLK_DELETE		= 127,
	/* End of ASCII mapped keysyms */

	/* International keyboard syms */
	SDLK_WORLD_0		= 160,		/* 0xA0 */
	SDLK_WORLD_1		= 161,
	SDLK_WORLD_2		= 162,
	SDLK_WORLD_3		= 163,
	SDLK_WORLD_4		= 164,
	SDLK_WORLD_5		= 165,
	SDLK_WORLD_6		= 166,
	SDLK_WORLD_7		= 167,
	SDLK_WORLD_8		= 168,
	SDLK_WORLD_9		= 169,
	SDLK_WORLD_10		= 170,
	SDLK_WORLD_11		= 171,
	SDLK_WORLD_12		= 172,
	SDLK_WORLD_13		= 173,
	SDLK_WORLD_14		= 174,
	SDLK_WORLD_15		= 175,
	SDLK_WORLD_16		= 176,
	SDLK_WORLD_17		= 177,
	SDLK_WORLD_18		= 178,
	SDLK_WORLD_19		= 179,
	SDLK_WORLD_20		= 180,
	SDLK_WORLD_21		= 181,
	SDLK_WORLD_22		= 182,
	SDLK_WORLD_23		= 183,
	SDLK_WORLD_24		= 184,
	SDLK_WORLD_25		= 185,
	SDLK_WORLD_26		= 186,
	SDLK_WORLD_27		= 187,
	SDLK_WORLD_28		= 188,
	SDLK_WORLD_29		= 189,
	SDLK_WORLD_30		= 190,
	SDLK_WORLD_31		= 191,
	SDLK_WORLD_32		= 192,
	SDLK_WORLD_33		= 193,
	SDLK_WORLD_34		= 194,
	SDLK_WORLD_35		= 195,
	SDLK_WORLD_36		= 196,
	SDLK_WORLD_37		= 197,
	SDLK_WORLD_38		= 198,
	SDLK_WORLD_39		= 199,
	SDLK_WORLD_40		= 200,
	SDLK_WORLD_41		= 201,
	SDLK_WORLD_42		= 202,
	SDLK_WORLD_43		= 203,
	SDLK_WORLD_44		= 204,
	SDLK_WORLD_45		= 205,
	SDLK_WORLD_46		= 206,
	SDLK_WORLD_47		= 207,
	SDLK_WORLD_48		= 208,
	SDLK_WORLD_49		= 209,
	SDLK_WORLD_50		= 210,
	SDLK_WORLD_51		= 211,
	SDLK_WORLD_52		= 212,
	SDLK_WORLD_53		= 213,
	SDLK_WORLD_54		= 214,
	SDLK_WORLD_55		= 215,
	SDLK_WORLD_56		= 216,
	SDLK_WORLD_57		= 217,
	SDLK_WORLD_58		= 218,
	SDLK_WORLD_59		= 219,
	SDLK_WORLD_60		= 220,
	SDLK_WORLD_61		= 221,
	SDLK_WORLD_62		= 222,
	SDLK_WORLD_63		= 223,
	SDLK_WORLD_64		= 224,
	SDLK_WORLD_65		= 225,
	SDLK_WORLD_66		= 226,
	SDLK_WORLD_67		= 227,
	SDLK_WORLD_68		= 228,
	SDLK_WORLD_69		= 229,
	SDLK_WORLD_70		= 230,
	SDLK_WORLD_71		= 231,
	SDLK_WORLD_72		= 232,
	SDLK_WORLD_73		= 233,
	SDLK_WORLD_74		= 234,
	SDLK_WORLD_75		= 235,
	SDLK_WORLD_76		= 236,
	SDLK_WORLD_77		= 237,
	SDLK_WORLD_78		= 238,
	SDLK_WORLD_79		= 239,
	SDLK_WORLD_80		= 240,
	SDLK_WORLD_81		= 241,
	SDLK_WORLD_82		= 242,
	SDLK_WORLD_83		= 243,
	SDLK_WORLD_84		= 244,
	SDLK_WORLD_85		= 245,
	SDLK_WORLD_86		= 246,
	SDLK_WORLD_87		= 247,
	SDLK_WORLD_88		= 248,
	SDLK_WORLD_89		= 249,
	SDLK_WORLD_90		= 250,
	SDLK_WORLD_91		= 251,
	SDLK_WORLD_92		= 252,
	SDLK_WORLD_93		= 253,
	SDLK_WORLD_94		= 254,
	SDLK_WORLD_95		= 255,		/* 0xFF */

	/* Numeric keypad */
	SDLK_KP0		= 256,
	SDLK_KP1		= 257,
	SDLK_KP2		= 258,
	SDLK_KP3		= 259,
	SDLK_KP4		= 260,
	SDLK_KP5		= 261,
	SDLK_KP6		= 262,
	SDLK_KP7		= 263,
	SDLK_KP8		= 264,
	SDLK_KP9		= 265,
	SDLK_KP_PERIOD		= 266,
	SDLK_KP_DIVIDE		= 267,
	SDLK_KP_MULTIPLY	= 268,
	SDLK_KP_MINUS		= 269,
	SDLK_KP_PLUS		= 270,
	SDLK_KP_ENTER		= 271,
	SDLK_KP_EQUALS		= 272,

	/* Arrows + Home/End pad */
	SDLK_UP			= 273,
	SDLK_DOWN		= 274,
	SDLK_RIGHT		= 275,
	SDLK_LEFT		= 276,
	SDLK_INSERT		= 277,
	SDLK_HOME		= 278,
	SDLK_END		= 279,
	SDLK_PAGEUP		= 280,
	SDLK_PAGEDOWN		= 281,

	/* Function keys */
	SDLK_F1			= 282,
	SDLK_F2			= 283,
	SDLK_F3			= 284,
	SDLK_F4			= 285,
	SDLK_F5			= 286,
	SDLK_F6			= 287,
	SDLK_F7			= 288,
	SDLK_F8			= 289,
	SDLK_F9			= 290,
	SDLK_F10		= 291,
	SDLK_F11		= 292,
	SDLK_F12		= 293,
	SDLK_F13		= 294,
	SDLK_F14		= 295,
	SDLK_F15		= 296,

	/* Key state modifier keys */
	SDLK_NUMLOCK		= 300,
	SDLK_CAPSLOCK		= 301,
	SDLK_SCROLLOCK		= 302,
	SDLK_RSHIFT		= 303,
	SDLK_LSHIFT		= 304,
	SDLK_RCTRL		= 305,
	SDLK_LCTRL		= 306,
	SDLK_RALT		= 307,
	SDLK_LALT		= 308,
	SDLK_RMETA		= 309,
	SDLK_LMETA		= 310,
	SDLK_LSUPER		= 311,		/* Left "Windows" key */
	SDLK_RSUPER		= 312,		/* Right "Windows" key */
	SDLK_MODE		= 313,		/* "Alt Gr" key */
	SDLK_COMPOSE		= 314,		/* Multi-key compose key */

	/* Miscellaneous function keys */
	SDLK_HELP		= 315,
	SDLK_PRINT		= 316,
	SDLK_SYSREQ		= 317,
	SDLK_BREAK		= 318,
	SDLK_MENU		= 319,
	SDLK_POWER		= 320,		/* Power Macintosh power key */
	SDLK_EURO		= 321,		/* Some european keyboards */
	SDLK_UNDO		= 322,		/* Atari keyboard has Undo */

	/* Add any other keys here */

	SDLK_LAST
};

enum SDLMod {
	KMOD_NONE  = 0x0000,
	KMOD_LSHIFT= 0x0001,
	KMOD_RSHIFT= 0x0002,
	KMOD_LCTRL = 0x0040,
	KMOD_RCTRL = 0x0080,
	KMOD_CTRL  = 0x00C0,
	KMOD_LALT  = 0x0100,
	KMOD_RALT  = 0x0200,
	KMOD_ALT   = 0x0300,
	KMOD_LMETA = 0x0400,
	KMOD_RMETA = 0x0800,
	KMOD_NUM   = 0x1000,
	KMOD_CAPS  = 0x2000,
	KMOD_MODE  = 0x4000,
	KMOD_RESERVED = 0x8000
};
enum SDL_GrabMode { SDL_GRAB_QUERY=-1, SDL_GRAB_OFF=0, SDL_GRAB_ON=1, SDL_GRAB_FULLSCREEN }
enum SDL_GLattr {
    SDL_GL_RED_SIZE,
    SDL_GL_GREEN_SIZE,
    SDL_GL_BLUE_SIZE,
    SDL_GL_ALPHA_SIZE,
    SDL_GL_BUFFER_SIZE,
    SDL_GL_DOUBLEBUFFER,
    SDL_GL_DEPTH_SIZE,
    SDL_GL_STENCIL_SIZE,
    SDL_GL_ACCUM_RED_SIZE,
    SDL_GL_ACCUM_GREEN_SIZE,
    SDL_GL_ACCUM_BLUE_SIZE,
    SDL_GL_ACCUM_ALPHA_SIZE,
    SDL_GL_STEREO,
    SDL_GL_MULTISAMPLEBUFFERS,
    SDL_GL_MULTISAMPLESAMPLES,
    SDL_GL_ACCELERATED_VISUAL,
    SDL_GL_SWAP_CONTROL
};
struct SDL_keysym { ubyte scancode; SDLKey sym; SDLMod mod; ushort unicode; }
SDL_VideoInfo *SDL_GetVideoInfo();
SDL_Surface * SDL_GetVideoSurface();
int SDL_WM_ToggleFullScreen(SDL_Surface *surface);
SDL_GrabMode SDL_WM_GrabInput(SDL_GrabMode mode);
int SDL_WM_IconifyWindow();
void SDL_WM_SetCaption(char *title, char *icon);
char *SDL_GetKeyName(SDLKey key);
int SDL_GL_SetAttribute(SDL_GLattr attr, int value);
int SDL_GL_GetAttribute(SDL_GLattr attr, int* value);
SDL_Surface * SDL_SetVideoMode(int width, int height, int bpp, uint flags);
void SDL_Quit();
char *SDL_GetError();
void SDL_GL_SwapBuffers();
int SDL_Init(uint flags);
uint SDL_GetTicks();
void SDL_ClearError();
void SDL_Delay(uint ms);
int SDL_PollEvent(SDL_Event *event);
int SDL_Flip(SDL_Surface *);
int SDL_LockSurface(SDL_Surface *);
void SDL_UnlockSurface(SDL_Surface *);
int SDL_UpperBlit(SDL_Surface *src, SDL_Rect *srcrect, SDL_Surface *dst, SDL_Rect *dstrect);
alias SDL_UpperBlit SDL_BlitSurface;
void SDL_UpdateRects (SDL_Surface *screen, int numrects, SDL_Rect *rects);
void SDL_UpdateRect (SDL_Surface *screen, int x, int y, uint w, uint h);
int SDL_FillRect (SDL_Surface *dst, SDL_Rect *dstrect, uint color);
uint SDL_MapRGB(SDL_PixelFormat *format, ubyte r, ubyte g, ubyte b);
void SDL_GetRGB(uint pixel, SDL_PixelFormat *fmt, ubyte *r, ubyte *g, ubyte *b);
uint SDL_MapRGBA(SDL_PixelFormat *format, ubyte r, ubyte g, ubyte b, ubyte a);
void SDL_GetRGBA(uint pixel, SDL_PixelFormat *fmt, ubyte *r, ubyte *g, ubyte *b, ubyte *a);

const ubyte SDL_APPMOUSEFOCUS=0x01;
const ubyte SDL_APPINPUTFOCUS=0x02;
const ubyte SDL_APPACTIVE=0x04;
const uint SDL_INIT_TIMER=0x00000001;
const uint SDL_INIT_AUDIO=0x00000010;
const uint SDL_INIT_VIDEO=0x00000020;
const uint SDL_INIT_CDROM=0x00000100;
const uint SDL_INIT_JOYSTICK=0x00000200;
const uint SDL_INIT_NOPARACHUTE=0x00100000;
const uint SDL_INIT_EVENTTHREAD=0x01000000;
const uint SDL_INIT_EVERYTHING=0x0000FFFF;

const uint SDL_SWSURFACE=0x00000000;
const uint SDL_HWSURFACE=0x00000001;
const uint SDL_ASYNCBLIT=0x00000004;
const uint SDL_ANYFORMAT=0x10000000;
const uint SDL_HWPALETTE=0x20000000;
const uint SDL_DOUBLEBUF=0x40000000;
const uint SDL_FULLSCREEN=0x80000000;
const uint SDL_OPENGL=0x00000002;
const uint SDL_OPENGLBLIT=0x0000000A;
const uint SDL_RESIZABLE=0x00000010;
const uint SDL_NOFRAME=0x00000020;
const uint SDL_HWACCEL=0x00000100; /* Blit uses hardware acceleration */
const uint SDL_SRCCOLORKEY=0x00001000; /* Blit uses a source color key */
const uint SDL_RLEACCELOK=0x00002000; /* Private flag */
const uint SDL_RLEACCEL=0x00004000; /* Surface is RLE encoded */
const uint SDL_SRCALPHA=0x00010000; /* Blit uses source alpha blending */
const uint SDL_PREALLOC=0x01000000; /* Surface uses preallocated memory */

const SDL_BUTTON_LEFT=1;
const SDL_BUTTON_MIDDLE=2;
const SDL_BUTTON_RIGHT=3;
const SDL_BUTTON_WHEELUP=4;
const SDL_BUTTON_WHEELDOWN=5;

struct SDL_RWops {
	/* Seek to 'offset' relative to whence, one of stdio's whence values:
		SEEK_SET, SEEK_CUR, SEEK_END
	   Returns the final offset in the data source.
  */
  int function(SDL_RWops *context, int offset, int whence) seek;
	/* Read up to 'num' objects each of size 'objsize' from the data
	   source to the area pointed at by 'ptr'.
	   Returns the number of objects read, or -1 if the read failed.
	*/
  int function(SDL_RWops *context, void *ptr, int size, int maxnum) read;
	/* Write exactly 'num' objects each of size 'objsize' from the area
	   pointed at by 'ptr' to data source.
	   Returns 'num', or -1 if the write failed.
  */
  int function(SDL_RWops *context, void *ptr, int size, int num) write;
	/* Close and free an allocated SDL_FSops structure */
	int function(SDL_RWops *context) close;
	uint type;
	union hidden {
    struct {
      ubyte *base;
      ubyte *here;
      ubyte *stop;
    }
    struct unknown {
      void *data1;
    }
	};
}
SDL_RWops *SDL_RWFromFile(char *file, char *mode);
SDL_RWops *SDL_RWFromMem(void *mem, int size);
int SDL_SaveBMP_RW(SDL_Surface *surface, SDL_RWops *dst, int freedst);
SDL_Surface *SDL_LoadBMP_RW(SDL_RWops *src, int freesrc);
SDL_Surface *SDL_CreateRGBSurface(uint flags, int width, int height, int depth, uint Rmask, uint Gmask, uint Bmask, uint Amask);
void SDL_FreeSurface(SDL_Surface*);


/// SDL audio
struct SDL_AudioSpec {
	int freq;		/* DSP frequency -- samples per second -- 44100? */
	ushort format;		/* Audio data format */
	ubyte  channels;	/* Number of channels: 1 mono, 2 stereo */
	ubyte  silence;		/* Audio buffer silence value (calculated) */
	ushort samples;		/* Audio buffer size in samples (power of 2) */
	ushort padding;		/* Necessary for some compile environments */
	uint size;		/* Audio buffer size in bytes (calculated) */
	/* This function is called when the audio device needs more data.
	   'stream' is a pointer to the audio data buffer
	   'len' is the length of that buffer in bytes.
	   Once the callback returns, the buffer will no longer be valid.
	   Stereo samples are stored in a LRLRLR ordering.
	*/
	void function(void *userdata, ubyte *stream, int len) callback;
	void *userdata;
}
int SDL_OpenAudio(SDL_AudioSpec *desired, SDL_AudioSpec *obtained);
void SDL_PauseAudio(int pause_on);
const AUDIO_U8=0x0008;
const AUDIO_S8=0x8008;
const AUDIO_U16LSB=0x0010;
const AUDIO_S16LSB=0x8010;
const AUDIO_U16MSB=0x1010;
const AUDIO_S16MSB=0x9010;
alias AUDIO_U16LSB AUDIO_U16;
alias AUDIO_S16LSB AUDIO_S16;

struct SDL_Cursor {
	SDL_Rect area;			/* The area of the mouse cursor */
	short hot_x, hot_y;		/* The "tip" of the cursor */
	ubyte *data;			/* B/W cursor data */
	ubyte *mask;			/* B/W cursor mask */
	ubyte *save[2];			/* Place to save cursor area */
	void *wm_cursor;		/* Window-manager cursor */
}
void *SDL_SetCursor(SDL_Cursor *);
