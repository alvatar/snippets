module xf.game.TimeHub;

private {
	import xf.game.TickTracker;
	import xf.game.Misc : tick;
	import xf.utils.Singleton;
}



private final class TimeHub : TickTracker {
	uint	ticksPerSecond() {
		return ticksPerSecond_;
	}
	
	
	double secondsPerTick() {
		return 1.0 / ticksPerSecond;
	}
	
	
	void overrideTicksPerSecond(uint tps) {
		ticksPerSecond_ = tps;
	}


	double secondsToTicks(double seconds) {
		return seconds * ticksPerSecond_;
	}
	
	
	double ticksToSeconds(uint ticks) {
		return cast(double)ticks / ticksPerSecond_;
	}
	
	
	tick currentTick() {
		return currentTick_;
	}
	
	
	void overrideCurrentTick(tick t) {
		//assert (t >= currentTick_);
		//inputTick_ = (t - currentTick_) + inputTick_;
		inputTick_ = t;
		currentTick_ = t;
	}
	
	
	void advanceTick(uint num = 1) {
		currentTick_ += num;
		foreach (t; trackers) {
			t.advanceTick(num);
		}
	}
	
	
	// trim event and state history by 'ticks' number of ticks
	void trimHistory(uint ticks) {
		foreach (t; trackers) {
			t.trimHistory(ticks);
		}
	}
	
	
	void rollback(uint ticks) {
		assert (currentTick_ >= ticks);
		currentTick_ -= ticks;
		
		foreach (t; trackers) {
			t.rollback(ticks);
		}
	}
	
	
	void addTracker(TickTracker t) {
		trackers ~= t;
	}
	
	
	/**
		How many inputs do we have buffered ahead of currentTick.
		In other words, the tick for which we have inputs
	*/
	tick inputTick() {
		return inputTick_;
	}
	
	
	synchronized void incInputTick() {
		++inputTick_;
		targetInputTickOffset_ -= 1.0;
	}
	
	
	/**
		How many ticks ahead of inputTick would we like to be.
		This will be useful for precise client/server synchronization, where the client should do catch-up and time speed manipulation
	*/
	double targetInputTickOffset() {
		return targetInputTickOffset_;
	}
	
	
	synchronized void setTargetInputTickOffset(double off) {
		targetInputTickOffset_ = off;
	}
	
	
	synchronized void incTargetInputTickOffset() {
		targetInputTickOffset_ += 1.0;
	}
	
	
	private {
		uint		ticksPerSecond_ = 100;
		tick		currentTick_;
		tick		inputTick_ = 0;
		double	targetInputTickOffset_ = 0.0;
		
		TickTracker[]	trackers;
	}
}


alias Singleton!(TimeHub)	timeHub;
