module xf.loader.scene.model.Entity;


interface Entity {
	char[]	name();
	void		name(char[]);
	
	char[]	type();
	void		type(char[]);
}



template MEntity() {
	char[]	name() {
		return _name;
	}
	
	void		name(char[] s) {
		_name = s;
	}
	
	char[]	type() {
		return _type;
	}
	
	void		type(char[] s) {
		_type = s;
	}

	
	protected {
		char[]	_name;
		char[]	_type;
	}
}

