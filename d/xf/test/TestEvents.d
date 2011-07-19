module test.TestEvents;

private {
	import game.Event;
	import game.Misc;
	import xpose.Expose;
}



class RegisterEventOrder : Order {
	int		id;
	char[]	name;
	mixin exposeEvent!(`id|name`);
}


class TestWish : Wish {
	int		a;
	float		b;
	char[]	c;
	mixin exposeEvent!(`a|b|c`);
}


class TestOrder : Order {
	char[]	str;
	mixin exposeEvent!(`str`);
}


class CreatePlayer : Order {
	objId	id;
	mixin exposeEvent!(`id`);
}
