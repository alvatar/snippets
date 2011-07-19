module xf.platform.win32.xinput;

private {
	import xf.platform.win32.windef;
	import tango.sys.SharedLib : SharedLib, SharedLibException;
	import tango.util.log.Trace;
}

// winerror.h
const ERROR_SUCCESS = 0L;
// end winerror

version (Windows) {
	char[] xinputDynLibFileName = "xinput1_3.dll";
} else {
	static assert(false, "TODO");
}

private {
	SharedLib xinputDynLib;
}

void initXInput() {
	if (xinputDynLib !is null)
		xinputDynLib.unload();

	try {
		xinputDynLib = SharedLib.load(xinputDynLibFileName);
		Trace.formatln("Loading: {} completed.", xinputDynLibFileName);
	}
	catch (SharedLibException e) {
		Trace.formatln("Warning: {}.", e.msg);
	}

	assert(xinputDynLib !is null, "Could't load xinput1_3.dll");

	loadXInputFunctions_(function void*(char* n) {
		return xinputDynLib.getSymbol(n);
	});
}

//
// Device types available in XINPUT_CAPABILITIES
//
const XINPUT_DEVTYPE_GAMEPAD         = 0x01;

//
// Device subtypes available in XINPUT_CAPABILITIES
//
const XINPUT_DEVSUBTYPE_GAMEPAD      = 0x01;
const XINPUT_DEVSUBTYPE_WHEEL        = 0x02;
const XINPUT_DEVSUBTYPE_ARCADE_STICK = 0x03;
const XINPUT_DEVSUBTYPE_FLIGHT_SICK  = 0x04;
const XINPUT_DEVSUBTYPE_DANCE_PAD    = 0x05;
const XINPUT_DEVSUBTYPE_GUITAR       = 0x06;
const XINPUT_DEVSUBTYPE_DRUM_KIT     = 0x08;

//
// Flags for XINPUT_CAPABILITIES
//
const XINPUT_CAPS_VOICE_SUPPORTED    = 0x0004;

//
// Constants for gamepad buttons
//
const XINPUT_GAMEPAD_DPAD_UP         = 0x0001;
const XINPUT_GAMEPAD_DPAD_DOWN       = 0x0002;
const XINPUT_GAMEPAD_DPAD_LEFT       = 0x0004;
const XINPUT_GAMEPAD_DPAD_RIGHT      = 0x0008;
const XINPUT_GAMEPAD_START           = 0x0010;
const XINPUT_GAMEPAD_BACK            = 0x0020;
const XINPUT_GAMEPAD_LEFT_THUMB      = 0x0040;
const XINPUT_GAMEPAD_RIGHT_THUMB     = 0x0080;
const XINPUT_GAMEPAD_LEFT_SHOULDER   = 0x0100;
const XINPUT_GAMEPAD_RIGHT_SHOULDER  = 0x0200;
const XINPUT_GAMEPAD_A               = 0x1000;
const XINPUT_GAMEPAD_B               = 0x2000;
const XINPUT_GAMEPAD_X               = 0x4000;
const XINPUT_GAMEPAD_Y               = 0x8000;


//
// Gamepad thresholds
//
const XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE  = 7849;
const XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE = 8689;
const XINPUT_GAMEPAD_TRIGGER_THRESHOLD    = 30;

//
// Flags to pass to XInputGetCapabilities
//
const XINPUT_FLAG_GAMEPAD            = 0x00000001;

//
// Devices that support batteries
//
const BATTERY_DEVTYPE_GAMEPAD        = 0x00;
const BATTERY_DEVTYPE_HEADSET        = 0x01;

//
// Flags for battery status level
//
const BATTERY_TYPE_DISCONNECTED      = 0x00;    // This device is not connected
const BATTERY_TYPE_WIRED             = 0x01;    // Wired device, no battery
const BATTERY_TYPE_ALKALINE          = 0x02;    // Alkaline battery source
const BATTERY_TYPE_NIMH              = 0x03;    // Nickel Metal Hydride battery source
const BATTERY_TYPE_UNKNOWN           = 0xFF;    // Cannot determine the battery type

// These are only valid for wireless, connected devices, with known battery types
// The amount of use time remaining depends on the type of device.
const BATTERY_LEVEL_EMPTY            = 0x00;
const BATTERY_LEVEL_LOW              = 0x01;
const BATTERY_LEVEL_MEDIUM           = 0x02;
const BATTERY_LEVEL_FULL             = 0x03;

// User index definitions
const XUSER_MAX_COUNT                = 4;

const XUSER_INDEX_ANY                = 0x000000FF;


//
// Codes returned for the gamepad keystroke
//

const VK_PAD_A                       = 0x5800;
const VK_PAD_B                       = 0x5801;
const VK_PAD_X                       = 0x5802;
const VK_PAD_Y                       = 0x5803;
const VK_PAD_RSHOULDER               = 0x5804;
const VK_PAD_LSHOULDER               = 0x5805;
const VK_PAD_LTRIGGER                = 0x5806;
const VK_PAD_RTRIGGER                = 0x5807;

const VK_PAD_DPAD_UP                 = 0x5810;
const VK_PAD_DPAD_DOWN               = 0x5811;
const VK_PAD_DPAD_LEFT               = 0x5812;
const VK_PAD_DPAD_RIGHT              = 0x5813;
const VK_PAD_START                   = 0x5814;
const VK_PAD_BACK                    = 0x5815;
const VK_PAD_LTHUMB_PRESS            = 0x5816;
const VK_PAD_RTHUMB_PRESS            = 0x5817;

const VK_PAD_LTHUMB_UP               = 0x5820;
const VK_PAD_LTHUMB_DOWN             = 0x5821;
const VK_PAD_LTHUMB_RIGHT            = 0x5822;
const VK_PAD_LTHUMB_LEFT             = 0x5823;
const VK_PAD_LTHUMB_UPLEFT           = 0x5824;
const VK_PAD_LTHUMB_UPRIGHT          = 0x5825;
const VK_PAD_LTHUMB_DOWNRIGHT        = 0x5826;
const VK_PAD_LTHUMB_DOWNLEFT         = 0x5827;

const VK_PAD_RTHUMB_UP               = 0x5830;
const VK_PAD_RTHUMB_DOWN             = 0x5831;
const VK_PAD_RTHUMB_RIGHT            = 0x5832;
const VK_PAD_RTHUMB_LEFT             = 0x5833;
const VK_PAD_RTHUMB_UPLEFT           = 0x5834;
const VK_PAD_RTHUMB_UPRIGHT          = 0x5835;
const VK_PAD_RTHUMB_DOWNRIGHT        = 0x5836;
const VK_PAD_RTHUMB_DOWNLEFT         = 0x5837;

//
// Flags used in XINPUT_KEYSTROKE
//
const XINPUT_KEYSTROKE_KEYDOWN       = 0x0001;
const XINPUT_KEYSTROKE_KEYUP         = 0x0002;
const XINPUT_KEYSTROKE_REPEAT        = 0x0004;

//
// Structures used by XInput APIs
//
struct XINPUT_GAMEPAD {
    WORD                                wButtons;
    BYTE                                bLeftTrigger;
    BYTE                                bRightTrigger;
    SHORT                               sThumbLX;
    SHORT                               sThumbLY;
    SHORT                               sThumbRX;
    SHORT                               sThumbRY;
}
alias XINPUT_GAMEPAD* PXINPUT_GAMEPAD;

struct XINPUT_STATE {
    DWORD                               dwPacketNumber;
    XINPUT_GAMEPAD                      Gamepad;
}
alias XINPUT_STATE* PXINPUT_STATE;

struct XINPUT_VIBRATION {
    WORD                                wLeftMotorSpeed;
    WORD                                wRightMotorSpeed;
}
alias XINPUT_VIBRATION *PXINPUT_VIBRATION;

struct XINPUT_CAPABILITIES {
    BYTE                                Type;
    BYTE                                SubType;
    WORD                                Flags;
    XINPUT_GAMEPAD                      Gamepad;
    XINPUT_VIBRATION                    Vibration;
}
alias XINPUT_CAPABILITIES *PXINPUT_CAPABILITIES;

struct XINPUT_BATTERY_INFORMATION {
    BYTE BatteryType;
    BYTE BatteryLevel;
}
alias XINPUT_BATTERY_INFORMATION *PXINPUT_BATTERY_INFORMATION;

struct XINPUT_KEYSTROKE {
    WORD    VirtualKey;
    WCHAR   Unicode;
    WORD    Flags;
    BYTE    UserIndex;
    BYTE    HidCode;
}
alias XINPUT_KEYSTROKE *PXINPUT_KEYSTROKE;

void loadXInputFunctions_(void* function(char*) loadFuncFromLib) {
	*cast(void**)&XInputGetState = loadFuncFromLib("XInputGetState");
	*cast(void**)&XInputSetState = loadFuncFromLib("XInputSetState");
	*cast(void**)&XInputGetCapabilities = loadFuncFromLib("XInputGetCapabilities");
	*cast(void**)&XInputEnable = loadFuncFromLib("XInputEnable");
//	*cast(void**)&XInputGetDSoundAudioDeviceGuids = loadFuncFromLib("XInputGetDSoundAudioDeviceGuids");
	*cast(void**)&XInputGetBatteryInformation = loadFuncFromLib("XInputGetBatteryInformation");
	*cast(void**)&XInputGetKeystroke = loadFuncFromLib("XInputGetKeystroke");
}
extern(System):
	DWORD function(DWORD dwUserIndex, XINPUT_STATE* pState) XInputGetState;
	DWORD function(DWORD dwUserIndex, XINPUT_VIBRATION* pVibration) XInputSetState;
	DWORD function(DWORD dwUserIndex, DWORD dwFlags, XINPUT_CAPABILITIES* pCapabilities) XInputGetCapabilities;
	void  function(BOOL enable) XInputEnable;
//	DWORD function(DWORD dwUserIndex, GUID* pDSoundRenderGuid, GUID* pDSoundCaptureGuid) XInputGetDSoundAudioDeviceGuids;
	DWORD function(DWORD dwUserIndex, BYTE devType, XINPUT_BATTERY_INFORMATION* pBatteryInformation) XInputGetBatteryInformation;
	DWORD function(DWORD dwUserIndex, DWORD dwReserved, PXINPUT_KEYSTROKE pKeystroke) XInputGetKeystroke;

