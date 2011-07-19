module xf.rt.TaskMngr;

private {
	import CPUid = xf.utils.CPUid;
	import xf.utils.Meta : Stuple, stuple;
	import tango.core.ThreadPool;
	import tango.core.Thread;
	import tango.core.Atomic;
	import tango.io.Stdout;
}




class TaskMngr {
	this() {
		_threads = CPUid.coresPerCPU;	
		Stdout.formatln("Detected {} cores", _threads);
		_pool = new ThreadPool!(int)(_threads);
	}
	
	
	int numTasks() {
		return _threads * _subdivs;
	}
	
	
	Stuple!(int, int) getSlice(int taskNr, int numElements) {
		return stuple(taskNr * numElements / numTasks, (taskNr+1) * numElements / numTasks);
	}
	
	
	void parallel(void delegate(int taskNr) dg) {
		Atomic!(int) taskCntr;
		taskCntr.store(0);
		
		void runTask(int nr) {
			scope (exit) taskCntr.increment();
			dg(nr);
		}
		
		for (int i = 0; i < numTasks; ++i) {
			_pool.assign(&runTask, i);
		}
		while (taskCntr.load() < numTasks) {
			Thread.yield;
		}
	}
	
	
	private {
		int					_threads;
		ThreadPool!(int)	_pool;
		
		const int 			_subdivs = 20;
	}
}
