/**
	Why bother driving the timer through the input subsystem ? The concept is that time should be an input.
	Generally everything that can vary between subsequent engine executions has to be an input.
	Why ?
	That gives a possibility to perfectly reproduce any execution process / create replays / etc
*/

module xf.game.MainProcess;


private {
	import xf.input.Input;
	
	import xf.core.Message;
	import xf.core.MessageHandler;
	import xf.core.MessageHub;

	import xf.core.InputHub;
	import xf.core.JobHub;
	
	import xf.utils.HardwareTimer;
	import xf.utils.Profiler : profile;
}


private class TimeReader : InputReader {
	void tick(TimeInput* i) {
		jobHub.update(cast(double)i.micros * 0.000001);
	}
	
	
	this() {
		registerReader!(TimeInput)(&this.tick);
	}
}


class MainProcess : Process {
	void exec() {
		HardwareTimer timer;

		void delegate() tick; {
			timer = new HardwareTimer;
			
			tick = () {
				TimeInput ti;
				ti.micros = timer.timeDeltaMicros();
				inputHub.timeChannel << ti;
			};
			
			// init the timer
			timer.timeDeltaMicros();
		}
		
		{
			MessageHandler handler = new MessageHandler;
			handler.register!(QuitMessage)(&this.handleQuitMsg);
			messageHub.registerMessageHandler(handler);
		}
		
		//timeHub();		// init the time hub.
		inputHub.timeChannel.addReader(new TimeReader);
		
		while (!stopped) {
			profile!(`jobHub`)({
				tick();
				inputHub.dispatchAll();
			});
		}
	}
	
	
	void handleQuitMsg(QuitMessage msg)	{
		stop();
	}
}
