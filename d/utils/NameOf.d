/**
	Based on the nameof module by Don Clugston
*/
module xf.utils.NameOf;

private {
	struct EstablishMangle {}
	struct inner(alias x) {}
	void outer(alias x)(inner!(x)) {}

	uint chopOffUint(ref char[] str) {
		uint res = 0;
		while (str.length > 0 && str[0] >= '0' && str[0] <= '9') {
			res *= 10;
			res += str[0] - '0';
			str = str[1..$];
		}
		return res;
	}
}



char[] mangleOf(alias func)() {
	char[] rawMangled = (&outer!(func)).mangleof;
	char[] modPrefix = EstablishMangle.mangleof[0..$-"15EstablishMangle".length];

	assert (rawMangled[0..2] == "PF");		// pointer to a function
	rawMangled = rawMangled[2..$];
	assert (rawMangled.length > modPrefix.length);
	assert (rawMangled[0..modPrefix.length] == modPrefix);
	rawMangled = rawMangled[modPrefix.length .. $];

	// we now have to parse the nr of chars in the symbol
	int symLength = chopOffUint(rawMangled);
	rawMangled = rawMangled[0..symLength];

	// we're in a template
	assert (rawMangled[0..3] == "__T");
	rawMangled = rawMangled[3..$];
	assert (rawMangled[0..6] == "5inner");
	rawMangled = rawMangled[6..$];

	// inner is a struct
	assert (rawMangled[0..1] == "S");
	rawMangled = rawMangled[1..$];

	symLength = chopOffUint(rawMangled);
	rawMangled = rawMangled[0..symLength];

	return rawMangled;
}


bool isStaticFuncMangle(char[] name) {
	assert (name[0..2] == "_D");	// can't determine it for non-d linkage
	name = name[2..$];
	int symLength = 0;
	while ((symLength = chopOffUint(name)) > 0) {
		name = name[symLength .. $];
	}
	return name[0] != 'M';
}


bool isStaticMemberFunc(alias func)() {
	return isStaticFuncMangle(mangleOf!(func)());
}
