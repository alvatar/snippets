module utils.test.cfgTest1;

import utils.Cfg;
import tango.io.protocol.Reader;
import tango.io.GrowBuffer;
import tango.io.Stdout;


void main() {
	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])
		`(mk-id stuff23ref, stuff2 = 'b c' /+/+/++/+/+/ = def, stuff3 /+ comment +/ stuff4)
		
		block Name {
				stuff1 = a
				(id stuff23ref)
				stuff5 {		// a nested tuple
					foo-bar = 1 2 3
				}
			}`c);
			
			
		scope loader = new CfgLoader;
		auto data = loader.load(r);
		
		Stdout.formatln("Parsing result:\n{}\n", data.toString);
		
		assert (data.length == 1);
		assert (Data.Type.Tuple == data[0].type);
		auto block = data[0].tuple;

		assert (block.length == 3);
			assert (Data.Type.String == block[0].type);
			assert (`block` == block[0]._string());
			assert (Data.Type.String == block[1].type);
			assert (`Name` == block[1]._string());
			assert (Data.Type.Tuple == block[2].type);
			
		auto tuple1 = block[2].tuple;
		assert (tuple1.length == 4);
			assert (Data.Type.AssignExpr == tuple1[0].type);
				assert (Data.Type.String == tuple1[0].assignExpr.lhs.type);
				assert (`stuff1` == tuple1[0].assignExpr.lhs._string());
				assert (Data.Type.String == tuple1[0].assignExpr.rhs.type);
				assert (`a` == tuple1[0].assignExpr.rhs._string());

			assert (Data.Type.AssignExpr == tuple1[1].type);
				assert (Data.Type.String == tuple1[1].assignExpr.lhs.type);
				assert (`stuff2` == tuple1[1].assignExpr.lhs._string());
				assert (Data.Type.AssignExpr == tuple1[1].assignExpr.rhs.type);
				auto stuff2 = tuple1[1].assignExpr.rhs.assignExpr;
					assert (Data.Type.String == stuff2.lhs.type);
					assert (`b c` == stuff2.lhs._string());
					assert (Data.Type.String == stuff2.rhs.type);
					assert (`def` == stuff2.rhs._string());

			assert (Data.Type.Tuple == tuple1[2].type);
			auto tuple2 = tuple1[2].tuple;
			assert (tuple2.length == 2);
				assert (Data.Type.String == tuple2[0].type);
				assert (`stuff3` == tuple2[0]._string());
				assert (Data.Type.String == tuple2[1].type);
				assert (`stuff4` == tuple2[1]._string());

			assert (Data.Type.Tuple == tuple1[3].type);
			auto tuple3 = tuple1[3].tuple;
			assert (tuple3.length == 2);
				assert (Data.Type.String == tuple3[0].type);
				assert (`stuff5` == tuple3[0]._string());
				assert (Data.Type.Tuple == tuple3[1].type);
				
				auto tuple4 = tuple3[1].tuple;
				assert (tuple4.length == 1);
				assert (Data.Type.AssignExpr == tuple4[0].type);
				auto foobar = tuple4[0].assignExpr;
				assert (Data.Type.String == foobar.lhs.type);
				assert (`foo-bar` == foobar.lhs._string());
				
				assert (Data.Type.Tuple == foobar.rhs.type);
				assert (3 == foobar.rhs.tuple.length);
	}


	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])`
dupa (foo omg crap
		crap)
		
		a { a b } c; d { e, f }
		
		1.234 0.2 shit 128
		
		(foo bar 'baz\nlol', omg)
		
		x={y (z 1 {w})}
		
		('funky stuff' 'here' 123.456 7)
`c);
			
			
		scope loader = new CfgLoader;
		auto data = loader.load(r);
		
		Stdout.formatln("Parsing result:\n{}\n", data.toString);
	}

	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])`
			(eval 'foo = 1	// lolz heh
			bar = /+ lol this
				is a comment
			geez +/ 2
			baz = 3')
			crap = 3.14159
			spam = { /+ another comment heh +/ ham /+
				zomg /+ a nesting one! +/
			+/ = bram; a = b; c = -123 } /+/+/+
		`c);
		
		void printHash(DataHash hash, char[][] keys ...) {
			foreach (k; keys) {
				Stdout.formatln(`hash[{}] = {}`, k, hash._string(k));
			}
		}
			
		auto data = (new CfgLoader).load(r);
		Stdout.formatln("Parsing result:\n{}\n", data.toString);
		scope hash = new DataHash(data);
		printHash(hash, `foo`, `bar`, `baz`, `crap`);
		scope spam = new DataHash(hash.tuple(`spam`));
		printHash(spam, `ham`, `a`, `c`);
	}


	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])`
			foo = 1	// lolz heh
			bar = /+ lol this
				is a comment
			geez +/ 2
			baz =
			crap = 3.14159
			spam = { /+ another comment heh +/ ham /+
				zomg /+ a nesting one! +/
			+/ = bram; a = b; c = -123 } /+/+/+
		`c);
		
		try {
			(new CfgLoader).load(r);
		} catch (Exception e) {
			Stdout.newline()(e.toString).newline;
		}
	}


	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])`foo = @`c);
		
		try {
			(new CfgLoader).load(r);
		} catch (Exception e) {
			Stdout.newline()(e.toString).newline;
		}
	}

	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])`(eval (file 'test1.cfg'))`c);
			
		scope loader = new CfgLoader;
		auto data = loader.load(r);
		
		Stdout.formatln("Parsing result:\n{}\n", data.toString);
	}

	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])
`(mk-id st1
	color = white;
)

(vbox
	a
	b
	c
)
`c);
			
		scope loader = new CfgLoader;
		auto data = loader.load(r);
		
		Stdout.formatln("Parsing result:\n{}\n", data.toString);
	}


	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])
`a = b c d
a = %{ b c d }
'''dupa
jasia
lol omg\nzomg'''`c);
			
		scope loader = new CfgLoader;
		auto data = loader.load(r);
		
		Stdout.formatln("Parsing result:\n{}\n", data.toString);
	}

	{
		auto c = new GrowBuffer;
		auto r = new Reader(c);
		
		c.write(cast(void[])
`
settings {
	display		1024 768 32
	fullscreen	false
}

(mk-id st1 %{
	color				white
	background-color	black

	layout {
		box { 5 10 5 10 } {
			title = '%{color red, text-weight bold}!!{}'
			%{color red} horizLine
			content = box { 0 0 0 0 }
		}
	}
})


// (id something) is a reference to some stuff created by the 'mk-id' directive
(id st1)	// style not directly following any item applies to what follows


slide {
	title 'OMG LOL RLY ? :D'
	content {
		img 'foobar.jpg'

		'''
			yea hai lol this is some wild content hai
			* no wai!
			* ttlly! {{ img 'dupa.png' }}
			* hah lol
				# zomg nowai
				# yawai
		'''
	}
}

%{some style stuff} slide {
	title 'some sample src code'
	content {
		code {
			src 'src/foo/bar.d'
			cmd 'build -full -clean -exec {}'
			lang D
		}
	}
}


(inline-id st1) slide {
	// more stuff
}
`c);
			
		scope loader = new CfgLoader;
		auto data = loader.load(r);
		
		Stdout.formatln("Parsing result:\n{}\n", data.toString);
	}
}
