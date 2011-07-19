module xf.prof.Main;

private {
	import xf.prof.Profiler;

	import xf.hybrid.Hybrid;
	import xf.hybrid.backend.GL;
	import xf.hybrid.Style;

	// for Thread.yield
	import tango.core.Thread;
	
	import tango.stdc.ctype : isgraph;
	import Array = tango.core.Array;
	import Path = tango.io.Path;
	import tango.io.device.File : File;
	import tango.math.Math;

	import tango.io.Stdout;
	import tango.stdc.stdio;
	import tango.text.convert.Format;
	
	version (MountZip) import tango.io.vfs.ZipFolder;
}



struct CallSpot {
	char[]	file;
	int		line;
	
	hash_t toHash() {
		return line + typeid(typeof(file)).getHash(&file);
	}
	
	int opEquals(CallSpot* cs) {
		return file == cs.file && line == cs.line;
	}
	
	int opCmp(CallSpot* cs) {
		if (cs.line > line) return 1;
		if (cs.line < line) return -1;
		return typeid(typeof(file)).compare(&file, &cs.file);
	}
}


class CallTree {
	char[]				name;
	int					numCalls;
	CallTree[char[]]	children;
	CallTree			parent;
	
	int[CallSpot]		callSpots;
	
	CallTree child(char[] name) {
		if (auto c = name in children) {
			return *c;
		} else {
			auto ct = new CallTree;
			children[name] = ct;
			ct.name = name;
			return ct;
		}
	}
	
	void setParents() {
		foreach (k, ref c; children) {
			c.parent = this;
			c.setParents();
		}
	}
}


char[] sanitizeString(char[] str) {
	char[] res = new char[str.length];
	foreach (i, c; str) {
		if (isgraph(c)) {
			res[i] = c;
		} else {
			res[i] = '?';
		}
	}
	return res;
}


CallTree buildCallTree(TraceInfo[] allInfo) {
	auto root = new CallTree;
	root.name = "root";
	
	foreach (info; allInfo) {
		struct CallData {
			char[]	func;
			char[]	file;
			int		line;
		}
		
		CallData[] callData;
		
		foreach(char[] func, char[] file, int line, ptrdiff_t offset, size_t address; info) {
			if (func !is null) {
				callData ~= CallData(sanitizeString(func), sanitizeString(file), line);
			}
		}
		callData.reverse;
		
		++root.numCalls;

		CallTree ct = root;
		bool gotMain = false;
		void next(CallData cd) {
			ct = ct.child(cd.func);
			++ct.numCalls;
			++ct.callSpots[CallSpot(cd.file, cd.line)];
		}
		foreach (cd; callData) {
			if (gotMain) {
				next(cd);
			} else {
				if (cd.func == "main") {
					gotMain = true;
					next(cd);
				}
			}
		}
		
		if (!gotMain) {
			ct = root;
			foreach (cd; callData) {
				next(cd);
			}
		}
	}
	
	root.setParents;
	return root;
}


/+struct FuncFile {
	char[]	func;
	char[]	file;

	hash_t toHash() {
		return typeid(typeof(func)).getHash(&func) + typeid(typeof(file)).getHash(&file);
	}
	
	int opEquals(FuncFile* cs) {
		return func == cs.func && file == cs.file;
	}
	
	int opCmp(FuncFile* cs) {
		int c1 = typeid(typeof(func)).compare(&func, &cs.func);
		if (c1 > 0) return 1;
		if (c1 < 0) return -1;
		return typeid(typeof(file)).compare(&file, &cs.file);
	}
}+/


class CallStats {
	CallTree		callTree;
	int[char[]]		spotsByFunc;
	int				totalSpots;
	
	
	this(TraceInfo[] allInfo) {
		callTree = buildCallTree(allInfo);
		totalSpots = allInfo.length;

		foreach (info; allInfo) {
			foreach(char[] func, char[] file, int line, ptrdiff_t offset, size_t address; info) {
				if (func !is null) {
					//++spotsByFunc[FuncFile(func, file)];
					++spotsByFunc[func];
				}
				break;
			}
		}
	}
}


struct CallTreeCommand {
	enum Type {
		LoadFile
	}
	
	Type		type;
	char[]	path;
	int		line;
}


char[] getSourcePath(char[] root_, char[] relPath_)
out (res) {
	assert (res.length < 2 || res[$-1] != '/');
} body {
	if (0 == relPath_.length) {
		relPath_ = ".";
	}
	if (0 == root_.length) {
		root_ = ".";
	}

	auto root		= Path.standard(root_.dup);
	auto relPath	= Path.standard(relPath_.dup);
	assert (!Path.parse(relPath).isAbsolute, relPath);
	
	void skipCur(ref char[] p) {
		if (p.length >= 2 && "./" == p[0..2]) {
			p = p[2..$];
		}
		if (p.length >= 1 && '.' == p[0]) {
			if (p.length < 2 || p[1] != '.') {
				p = p[1..$];
			}
		}
		if (p.length > 1 && '/' == p[$-1]) {
			p = p[0..$-1];
		}
	}
	
	skipCur(root);
	skipCur(relPath);
	
	while (relPath.length > 1 && relPath[0..2] == ".." && root.length > 0) {
		if (root.length >= 2 && root[$-2..$] == "..") {
			root = root ~ "/..";
		} else {
			root = Path.pop(root);
		}
		relPath = relPath[2..$];
		if (relPath.length > 0 && '/' == relPath[0]) {
			relPath = relPath[1..$];
		}
	}
	
	return Path.join(root, relPath);
}


void loadFile(SciEditor sci, char[] procName, char[] path, int line) {
	path = Path.standard(path);
	procName = Path.standard(procName);
	char[] procFolder = Path.pop(procName);
	
	//Stdout.formatln("{}:{}, {}:{}", path, Path.parse(path).isAbsolute, procName, Path.parse(procName).isAbsolute);

	char[] srcFile;
	if (Path.parse(path).isAbsolute) {
		srcFile = path;
	} else {
		if (!Path.parse(path).isAbsolute && Path.parse(procFolder).isAbsolute) {
			srcFile = getSourcePath(procFolder, path);
		}
	}
	
	if (srcFile !is null && srcFile.length > 2 && srcFile[$-2..$] == ".d") {
		char[] contents;
		try {
			contents = cast(char[])File.get(srcFile);
		} catch (Exception e) {
			contents = Format("Unable to load {}: {}", srcFile, e.toString);
			line = 1;
		}
		sci.text = contents;
		sci.line = line-1;
	}
}


void main(char[][] args) {
	if (args.length < 2) {
		Stdout.formatln("Usage: {} [processName]", args[0]);
		return;
	}
	
	auto prof = new Profiler;

	ProcessInfo proc = prof.processes().findProcess(args[1]);
	if (proc.valid) {
		prof.attach(proc);
		prof.attachToAllThreads();
		prof.sampleDelayMicros = 5000;
		prof.threadPriority = ThreadPriority.Higher;
		prof.startSampling;
		
		Stdout.formatln(\n"Sampling the process. Hit ENTER to stop.");
		getchar();
		
		// ...
		
		prof.stopSampling;
		
		CallStats[int] callStats;
		CallStats getCallStats(int i) {
			if (auto res = i in callStats) {
				return *res;
			} else {
				return callStats[i] = new CallStats(prof.results(i).info.traceInfo);
			}
		}

		version (MountZip) gui.vfs.mount(new ZipFolder("GUI.zip"));
		version (DontMountExtra) {} else gui.vfsMountDir(`../hybrid`);
		scope cfg = loadHybridConfig(`./Main.gui`);
		scope renderer = new Renderer;
		
		bool programRunning = true;
		while (programRunning) {
			gui.begin(cfg);
				if (gui().getProperty!(bool)("wnd.frame.closeClicked")) {
					programRunning = false;
				}
				
				Group(`main`) [{
					gui.push(`main`);
					
					auto tv = TabView(`threadTabs`);
					if (!tv.initialized) {
						for (int i = 0; i < prof.numThreads; ++i) {
							tv.label[i] = Format("Thread {}", i);
						}
					}
					
					CallTreeCommand[] cmds;
					
					tv [{
						auto vb = VBox(tv.activeTab) [{
							cmds = drawCallTree(getCallStats(tv.activeTab));
						}];
						
						if (!vb.initialized) {
							vb.cfg(`
							layout = {
								padding = 5 5;
							}
							style.normal = {
								background = solid(rgb(0.1, 0.1, 0.1));
								border = 1 rgb(.5, .5, .5);
							}`);
						}
					}];
					
					auto codeSci = SciEditor(`codeSci`);

					foreach (cmd; cmds) {
						switch (cmd.type) {
							case CallTreeCommand.Type.LoadFile: {
								loadFile(codeSci, proc.filePath, cmd.path, cmd.line);
							} break;
						}
					}
					
					gui.pop;
				}];
			gui.end();
			gui.render(renderer);
			Thread.yield();
		}
	}
}


CallTreeCommand[] drawCallTree(CallStats callStats) {
	CallTreeCommand[] cmds;
	
	ScrollView nameScroll;
	
	// useChildSize will make the ScrollView expand in the X axis
	ScrollView().useChildSize(1) [{
		alias CallTree TreeKey;
		
		// The two columns for TreeView, separated by 20 pixels' gap
		HBox().open;
		auto sv = nameScroll = ScrollView();
		sv.open;
		auto vbox0 = VBox();
		gui.close;
		sv.useChildSize = 2;
		sv.userSize = vec2(640, 0);
		Dummy().userSize = vec2(20, 0);
		auto vbox1 = VBox();
		gui.close;

		TreeView().doGUI(
			// add the first widget generated by the tree view to vbox0, the second to vbox1
			(int row, int col) {
				return cast(IWidget)(0 == col ? vbox0 : vbox1);
			},
			
			// start with the parent directory
			callStats.callTree,
			
			// entry for the tree view data function
			(TreeKey key, TreeContext ctx) {
				float callsFrac = 1.f;
				if (key.parent !is null) {
					callsFrac = cast(float)key.numCalls / key.parent.numCalls;
				}
				
				float intens = sqrt(callsFrac);
				float totalIntens = sqrt(cast(float)key.numCalls / callStats.callTree.numCalls);
				float gsIntens = 0.f;
				
				int globalSpots = 0;
				if (auto gs = key.name in callStats.spotsByFunc) {
					globalSpots = *gs;
					gsIntens = cast(float)globalSpots / callStats.totalSpots;
					gsIntens = sqrt(sqrt(gsIntens));
				}
				
				vec4 labelColor = vec4(.4, .4, .4, 1);
				labelColor.r = labelColor.r * (1.f - totalIntens) + totalIntens;
				labelColor.g = labelColor.g * (1.f - intens) + intens;
				
				labelColor = (1.f - gsIntens) * labelColor + gsIntens * vec4(1, .2, 1, 1);
					
				auto displayPopup = ctx[new bool];
				auto rclicked = ctx[new bool];
				
				class HoverHandler {
					bool* displayPopup;
					bool* rclicked;

					EventHandling handleMouseButton(MouseButtonEvent e) {
						if (MouseButton.Right == e.button) {
							if (e.down && e.sinking) {
								*rclicked = true;
								//assert (false);
							}
							return EventHandling.Stop;
						}
						return EventHandling.Continue;
					}

					EventHandling handleMouseEnter(MouseEnterEvent e) {
						*displayPopup = true;
						return EventHandling.Continue;
					}
					
					EventHandling handleMouseLeave(MouseLeaveEvent e) {
						*displayPopup = false;
						return EventHandling.Continue;
					}
				}
				
				vec2 popupPos = vec2.zero;

				// the first column will contain the func name
				with (Label()) {
					text = key.name;
					style.color.value = labelColor;
					
					if (!initialized) {
						auto hh = new HoverHandler;
						hh.displayPopup = displayPopup;
						hh.rclicked = rclicked;
						addHandler(&hh.handleMouseEnter);
						addHandler(&hh.handleMouseLeave);
						addHandler(&hh.handleMouseButton);
					}
					
					popupPos = globalOffset;
					popupPos.x += 2;
					popupPos.y += size.y + 2;
				}
				
				auto spots = ctx[{
					struct Spot {
						int			numCalls;
						CallSpot	cs;
					}
					
					Spot[] spots;
					foreach (cs, numCalls; key.callSpots) {
						spots ~= Spot(numCalls, cs);
					}
					
					Array.sort(spots, (ref Spot a, ref Spot b) {
						return b.numCalls < a.numCalls;
					});
					
					if (spots.length > 10) {
						spots.length = 10;
					}
					
					return spots;
				}()];
				
				auto spotLabels = ctx[{
					char[][] res;
					foreach (cs; spots) {
						res ~= Format("{} hits @ {} ({})", cs.numCalls, cs.cs.file, cs.cs.line).dup;
					}
					return res;
				}()];
				
				if (*rclicked) {
					*rclicked = false;
					if (spots.length > 0) {
						cmds ~= CallTreeCommand(CallTreeCommand.Type.LoadFile, spots[0].cs.file, spots[0].cs.line);
					}
				}
				
				if (*displayPopup) {
					BackgroundStyle popupBack;
					popupBack.type = BackgroundStyle.Type.Solid;
					popupBack.Solid = vec4(0, 0, 0, 0.8);
					
					//auto grp = Group(`.overlay`); grp [{
					gui.openOverlay();
						with (VBox().cfg(`layout = { padding = 5 5; }`) [{
							with (Label()) {
								text = "local";
								fontSize = 9;
							}
							with (Label()) {
								text = ctx[Format("{}% tree / {}% total / {} calls",
									100 * callsFrac,
									100 * cast(float)key.numCalls / callStats.callTree.numCalls,
									key.numCalls
								).dup];
								fontSize = 14;
							}

							{
								with (Label()) {
									text = "global";
									fontSize = 9;
								}
								with (Label()) {
									text = ctx[Format("{}% total / {} spots",
										100 * cast(float)globalSpots / callStats.totalSpots,
										globalSpots
									).dup];
									fontSize = 14;
								}
							}
							
							if (spots.length > 1) {
								with (Label()) {
									text = ctx[Format(
										"Top {} spots:", spots.length
									).dup];
									fontSize = 14;
								}
							}
							
							if (spots.length > 0) {
								Dummy().userSize = vec2(0, 5);
							}
							
							foreach (i, s; spotLabels) {
								Label(i).text = s;
							}
						}]) {
							parentOffset = popupPos;// - grp.globalOffset;
							style.background.value = popupBack;
						}
					//}];
					gui.close;		// overlay
				}

				// the second column will contain the number of calls
				with (Label()) {
					text = ctx[Format("{}% : {} calls", 100 * callsFrac, key.numCalls).dup];
					style.color.value = labelColor;
				};
				
				auto children = ctx[{
					auto res = key.children.values;
					Array.sort(res, (ref CallTree a, ref CallTree b) {
						return b.numCalls < a.numCalls;
					});
					return res;
				}()];
				
				foreach (ch; children) {
					ctx.recurse(ch);
				}
			}
		);
	}].userSize = vec2(0, 380);
	
	auto nameHScrol = HScrollbar();
	nameScroll.overrideHScroll = nameHScrol;
	nameHScrol.layoutAttribs = "hexpand hfill";
	
	return cmds;
}
