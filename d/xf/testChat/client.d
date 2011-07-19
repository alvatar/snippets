private {
	import game.Event;
	import game.GameInterface;
	import net.GameClient;
	import net.RaknetClient;
	import game.TimeHub;

	import events;
	import std.c.time : usleep;
}

private {
	import hybrid.Slot;
	import hybrid.SlotRegistration;
	import hybrid.All;
	import hybrid.AllSlots;
	import hybrid.Context;
	import hybrid.GuiRenderer;
	import hybrid.SdlGlRenderer;
	import hybrid.SimpleInputReader;
	import hybrid.GuiInputReader;
	import hybrid.IconCache;
	import hybrid.Font : Font, FontMngr;
	import maths.Vec;
	
	import std.stdio;
}


void doGui(dgType)(void delegate() initDg, dgType dg, void delegate(GuiRenderer, GuiInputReader) postDg = null) {
	initializeGuiDefaults();
	auto renderer = new SdlGlRenderer;
	renderer.xResolution = 320;
	renderer.yResolution = 220;
	renderer.winTitle = `deadChat`;
	auto inputReader = new SimpleInputReader(guiContext);
	renderer.initialize();

	renderer.hackUpdate();
	renderer.hackSetInputReader(inputReader);
	
	if (initDg !is null) initDg();
	
	//for (int i = 0; i < 2; ++i) {
	while (true) {
		guiContext.disconnect();

		static if (is(dgType == void delegate(GuiRenderer))) {
			dg(renderer);
		} else {
			static assert (is(dgType == void delegate()));
			dg();
		}

		guiContext.clean();
		guiContext.reset();
		guiContext.flush();
		guiContext.draw(renderer);
		
		if (postDg) postDg(renderer, inputReader);
		renderer.drawAll();
		
		if (!renderer.hackUpdate()) break;
	}
}


class EnterCatcher : VBox {
	mixin MSlot;
	mixin MSlotContext;
	
	bool wrapperKeyHandler(KeyboardEvent e) {
		if (e.bubbling) return false;
		if (!e.down) return false;
		if (e.keySym != e.keySym.K_ENTER) return false;
		handler();
		return true;
	}
	
	this() {
		addHandler(&wrapperKeyHandler);
	}
		
	void delegate() handler;
}


void main() {
	auto game = new GameInterface;
	auto client = new GameClient((new RaknetClient).connect(0, `86.3.21.0`, 8000));
	
	TextBox 		output;
	InputBox		input;
	InputBox		nickInput;
	char[][ubyte]	playersConnected;
	ubyte			myId;
	char[]			myNick;
	bool				connected = false;
	bool				loggingIn = false;
	bool				loggedIn = false;
	
	IncomingMsg.addHandler((IncomingMsg e) {
		if (output !is null) {
			output.addLine(toUTF32(`<` ~ playersConnected[e.pid] ~ `> ` ~ e.text));
		}
	});
	
	LoginRequestAccepted.addHandler((LoginRequestAccepted e) {
		playersConnected[myId = e.pid] = myNick;
		loggedIn = true;
	});
	
	LoginRequestRejected.addHandler((LoginRequestRejected e) {
		loggingIn = false;
		if (nickInput) nickInput.text = e.reason;
	});

	PlayerLogin.addHandler((PlayerLogin e) {
		playersConnected[e.pid] = e.nick;
		if (output !is null) {
			output.addLine(toUTF32(e.nick ~ ` joins the chat.`));
		}
	});

	PlayerDisconnection.addHandler((PlayerDisconnection e) {
		if (output !is null) {
			output.addLine(toUTF32(playersConnected[e.pid] ~ ` leaves the chat.`));
		}
		playersConnected.remove(e.pid);
	});

	client.registerConnectionHandler({
		connected = true;
	});
	
	doGui({}, {
		Fill(`color = 0.27 0.3 0.3 0.8`) [{
			EnterCatcher() [{
				if (loggingIn) {
					if (loggedIn) {
						guiContext.mark(1000);
						
						output = TextBox();
						input = InputBox();
					} else {
						guiContext.mark(2000);
						Label(`text = 'logging in...'`);
					}
				} else {
					guiContext.mark(3000);
					
					Border(`padding = 5 5`) [{
						HBox(`spacing = 5`) [{
							nickInput = InputBox(`text = 'your nick'`);
							
							if (Button(`text = 'login'`).clicked && !loggingIn) {
								LoginRequest(myNick = nickInput.text).immediate();
								loggingIn = true;
							}
						}];
					}];
				}
			}].handler = {
				if (loggedIn && output && input) {
					OutgoingMsg(input.text).immediate();
					input.text = ``;
				}
			};
		}];

		{
			static int blah = 0;
			if (blah++ % 10 == 0) {
				client.receiveData();
				timeHub.advanceTick();
				printf(`Tick: %d`\n, timeHub.currentTick);
			}
		}
		
		usleep(10000);
	});
}
