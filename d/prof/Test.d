module xf.prof.Test;

import xf.prof.Profiler;
import xf.prof.Demangler;

import tango.io.Stdout;
import tango.stdc.stdio;



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
		
		auto results = prof.results(0);
		foreach (i, info; results.info.traceInfo) {
			Stdout.formatln("{}:", i);
			foreach (x; info) {
				Stdout.formatln("{}", x);
			}
		}
	}
}
