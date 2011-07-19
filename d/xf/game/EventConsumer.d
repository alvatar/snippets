module xf.game.EventConsumer;

private {
	import xf.game.Event;
	import xf.game.Misc : tick;
}



interface EventConsumer {
	void	consume(Event, tick target);
}
