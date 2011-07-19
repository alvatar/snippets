module xf.utils.Graph;

private {
	import xf.utils.Memory;
}



enum CycleHandlingMode {
	Fail,
	AnyOrder
}


/**
	Topological sorting for graphs. Works for DAGs and generic directed graphs.
	
	It's a bit smarter than the average topological sorting, as it can provide the result in chunks of nodes
	that may be visited at the same time.
	
	Implementation note:
		It's a hybrid of topological and breadth-first sorting
	
	Params:
		nodeIter			= should provide a delegate to iterate over all graph nodes and yield their indices
		nodeSuccIter		= should provide a delegate to iterate over all successors of a specic node
		order				= a pre-allocated array to yield the result in. Should have as many items as there are nodes in the graph
		cycleHandling	= AnyOrder => allows cyclic graphs; Fail => fails on cycles
		clusterSink		= an optional delegate that will receive the result in clusters
		
	Returns:
		number of nodes ordered
*/
int findTopologicalOrder(
	void delegate(void delegate(int)) nodeIter,
	void delegate(int, void delegate(int)) nodeSuccIter,
	int[] order,
	CycleHandlingMode cycleHandling = CycleHandlingMode.Fail,
	void delegate(int clusterId, int[] cluster) clusterSink = null
) {
	int added = 0;
	int[] numIncoming;
	
	numIncoming.alloc(order.length);
	scope (exit) {
		numIncoming.free();
	}
	
	int maxNode = -1;
	nodeIter((int ni) {
		if (ni > maxNode) {
			maxNode = ni;
		}
		
		nodeSuccIter(ni, (int succ) {
			++numIncoming[succ];
		});
	});
	
	if (maxNode+1 != order.length) {
		throw new Exception("xf.utils.Graph.findTopologicalOrder: errorneous order.length");
	}
	
	int prevCluster = 0;
	int clusterId = 0;
	
findTopologicalOrderStart:
	foreach (ni, inc; numIncoming) {
		if (0 == inc) {
			order[added++] = ni;
		}
	}

	int nextCluster = added;
	void processCluster(int done) {
		if (done == nextCluster) {
			int[] cluster = order[prevCluster .. nextCluster];
			if (clusterSink !is null) {
				clusterSink(clusterId, cluster);
			}
			++clusterId;
			prevCluster = nextCluster;
			nextCluster = added;
		}
	}
	
	for (int done = 0; done < added; ++done) {
		processCluster(done);
		int ni = order[done];
		nodeSuccIter(ni, (int succ) {
			if (0 == --numIncoming[succ]) {
				order[added++] = succ;
			}
		});
	}
	processCluster(added);
	
	if (added != order.length) {
		switch (cycleHandling) {
			case CycleHandlingMode.Fail: {
				throw new Exception("xf.utils.Graph.findTopologicalOrder: the graph is not a DAG");
			} break;
			
			case CycleHandlingMode.AnyOrder: {
				int minIncoming = int.max;
				foreach (inc; numIncoming) {
					if (inc < minIncoming) {
						minIncoming = inc;
					}
				}
				
				foreach (ref inc; numIncoming) {
					inc -= minIncoming;
				}
				
				goto findTopologicalOrderStart;		// retry by resolving cycles in any order
			} break;
		}
	}
	
	return added;
}
