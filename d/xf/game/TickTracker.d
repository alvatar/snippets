module xf.game.TickTracker;


interface TickTracker {
	void advanceTick(uint ticks);
	void trimHistory(uint ticks);
	void rollback(uint ticks);
}
