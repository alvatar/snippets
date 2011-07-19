module xf.zig.Main;

private {
	import xf.zig.MainProcess;
	import xf.zig.Events;
	import xf.zig.Game : Game;
	import xf.zig.Input;
	
	import xf.game.Event;
	import xf.game.GameInterface;
	
	import xf.net.GameClient;
	import xf.net.RaknetClient;
	
	import xf.dog.Dog;
	import xf.dog.backend.Native;

	import xf.core.GraphicsHub;
	import xf.core.InputHub;
	import xf.core.JobHub;
	
	import xf.input.Input;
	import xf.input.KeySym;
	import xf.input.Joystick;
	
	import xf.input.Writer;
	
	import xf.utils.Bind;

	import tango.core.Thread;
	import tango.text.convert.Integer : toString;

	extern (C) int printf(char* format, ...);
}


GameClient				client;
PlayerInputSampler	playerInputSampler;


class PlayerInputReader : InputReader {
	void handle(PlayerInput* i) {
		tick targetTick = cast(tick)(timeHub.inputTick + timeHub.secondsToTicks(0.03));
		
		InputWish(i.thrust, i.strafe, i.rot,
			(i.shoot ? InputWish.Shoot : 0)
		).atTick(targetTick);		// + 30 ms
		
		if (i.focusShield8 != -1) {
			ShieldFocusWish(i.focusShield8).atTick(targetTick);
		}
	}
	
	this() {
		registerReader!(PlayerInput)(&this.handle);
	}
}


void updateGame() {
	standardClientUpdate(client, {
		playerInputSampler.sample();
	});
	synchronizeNetworkTicks(client);
}


void main(char[][] args) {
	auto context = GLWindow(); {
		context.title(`Zig!`).width(640).height(480).colorBits(24).depthBits(16).create();
		graphicsHub.context = context;
		
		use (context) in (GL gl) {
			gl.MatrixMode(GL_PROJECTION);
			gl.LoadIdentity();
			gl.gluOrtho2D(-64, 64, -48, 48);
			gl.MatrixMode(GL_MODELVIEW);
		};
	}
	
	// Install a keyboard input handler
	version (Windows) {
		context.msgFilter = &(new OSInputWriter(inputHub.mainChannel, false)).filter;
	} else {
		static assert (false);
	}
	
	auto gameInterface = new LoggingGameInterface;
	auto game = new Game!(false)(gameInterface);		// the Zig gameInterface
	client = new GameClient((new RaknetClient).connect(0, args.length > 1 ? args[1] : `localhost`, 8000));
	gameInterface.addObserver(client);
	
	foreach (info; Joystick) {
		printf(`joy name: %.*s; axes: %d; buttons: %d`\n, info.name, info.numAxes, info.numButtons);
		jobHub.addRepeatableJob(bind((Joystick joy) {
			JoystickInput inp;
			joy.readState(&inp.axes, &inp.buttons, &inp.pov);
			inputHub.mainChannel  << inp;
		}, new Joystick(info.id)).ptr(), timeHub.ticksPerSecond);
	}
	
	game.setLevel(`levels/ctf3.txt`);
	game.start();
	
	int playerNameId = 1;		// for further login requests when the name is already used
	
	client.registerConnectionHandler({
		LoginRequest(`Player1`).immediate;
	});
	
	LoginRejected.addHandler((LoginRejected e) {
		LoginRequest(`Player` ~ toString(++playerNameId)).immediate;		// hack for now
	});
	
	LoginAccepted.addHandler((LoginAccepted e) {
		playerInputSampler
			.outgoing.addReader(new PlayerInputReader);		// start reading inputs
			
		JoinGame().immediate;		// ask the server for state snapshots
	});
	
	
	{
		auto map = new PlayerInputMap(inputHub.mainChannel);
		map.outgoing.addReader(playerInputSampler = new PlayerInputSampler);
		jobHub.addRepeatableJob(&map.update, timeHub.ticksPerSecond);
	}
	
	jobHub.addRepeatableJob({ context.update(); }, timeHub.ticksPerSecond);
	jobHub.addPostFrameJob({ Thread.getThis.yield(); });

	jobHub.addRepeatableJob(&updateGame, timeHub.ticksPerSecond);
	jobHub.exec(new MainProcess);
}
