module xf.zig.Input;

private {
	import xf.input.Input;
	import xf.input.KeySym;
}



struct PlayerInput {
	byte thrust;
	byte strafe;
	byte rot;
	bool shoot;
	byte focusShield8 = -1;
	mixin MInput;
}

byte maxThrust = 127;


class PlayerInputMap : InputConverter {
	class Map : InputReader {
		bool	keys[512];
		bool	joyFire = false;
		
		void onInput(KeyboardInput* i) {
			keys[i.keySym] = i.type == i.type.Down ? true : false;
		}
		
		void feedKeyboardInput() {
			PlayerInput pinput;
			if (keys['w'])	pinput.thrust += maxThrust;
			if (keys['s'])	pinput.thrust -= maxThrust;
			if (keys['a'])	pinput.strafe -= maxThrust;
			if (keys['d'])	pinput.strafe += maxThrust;
			
			if (keys[KeySym.space]) {	// this should be covered by rate of fire calcs
				pinput.shoot = true;
				keys[KeySym.space] = false;
			}
			
			outgoing << pinput;
			outgoing.dispatchOne();
		}
		
		void onInput(JoystickInput* i) {
			PlayerInput pinput;
			
			pinput.thrust = cast(byte)((-i.axes[1] + -i.axes[3]) / 2 * maxThrust);
			pinput.rot = cast(byte)(-i.axes[4] * maxThrust);
			pinput.strafe = cast(byte)(i.axes[0] * maxThrust);
			
			if (!joyFire && (i.buttons || i.axes[2] < -0.2f)) {		// this should be covered by rate of fire calcs
				joyFire = true;
				pinput.shoot = true;
			} else if (!i.buttons && i.axes[2] > -0.2f) joyFire = false;
			
			if (i.pov != -1) {
				pinput.focusShield8 = (0 == i.pov)	?	0
																	:	(36_000 - i.pov) / 100 / (360 / 8);
			}
			
			outgoing << pinput;
			outgoing.dispatchOne();
		}

		this() {
			registerReader!(KeyboardInput)(&this.onInput);
			registerReader!(JoystickInput)(&this.onInput);
		}
	}
	
	
	void update() {
		map.feedKeyboardInput();
	}


	this(InputChannel incoming) {
		super(incoming);
		incoming.addReader(this.map = new Map);
	}
	
	
	private Map map;
}


class PlayerInputSampler : InputReader {
	void onInput(PlayerInput* inp) {
		byte abs(byte a) { return a > 0 ? a : -a; }
		if (abs(inp.thrust) > abs(input.thrust)) input.thrust = inp.thrust;
		if (abs(inp.strafe) > abs(input.strafe)) input.strafe = inp.strafe;
		if (abs(inp.rot) > abs(input.rot)) input.rot = inp.rot;
		if (inp.focusShield8 != -1) input.focusShield8 = inp.focusShield8;
		input.shoot |= inp.shoot;
	}


	this() {
		registerReader!(PlayerInput)(&this.onInput);
		outgoing = new InputChannel;
	}
	
	
	void sample() {
		outgoing << input;
		outgoing.dispatchOne();
		input = input.init;
	}
	

	PlayerInput	input;
	InputChannel	outgoing;
}
