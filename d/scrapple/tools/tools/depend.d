module tools.depend;
/* dependency graph modelling */

import tools.functional;

interface Node { }
interface OnceNode : Node { } // only traverse once!

class Graph {
  Node[string] nodes;
  bool[Node][Node] needs; // dependencies set
  Node lookup(string s) {
    if (auto p = s in nodes) return *p;
    else throw new Exception("No such node: "~s);
  }
  string rlookup(Node n) {
    foreach (key, value; nodes) if (value is n) return key;
    return "Unknown node";
  }
  Node[] getNeeds(Node n) {
    if (auto p = n in needs) return (*p).keys;
    else return null;
  }
  void addNode(string name, Node n, string[] deps...) {
    nodes[name] = n;
    if (n /notin/ needs) needs[n] = Init!(bool[Node]);
    auto need = n in needs;
    foreach (n2; deps /map/ &lookup)
      (*need)[n2] = true;
  }
  void addDepend(string from, string on) {
    needs[from.lookup()][on.lookup()] = true;
  }
  void traverse(void delegate(Node, void delegate(string)) dg) {
    bool[Node] left, finished;
    foreach (key, value; nodes) left[value] = true;
    Node current;
    if (left.length) current = left.keys[0];
    bool[Node] cycle_check; // cycle detection, brute-force
    while (current) {
      if (current in cycle_check)
        throw new Exception("Dependency cycle in "~rlookup(current)~"!");
      cycle_check[current] = true;
      auto needs = getNeeds(current) /select/ (Node n) { return n /notin/ finished; };
      if (needs.length) {
        current = needs[0];
        continue;
      }
      cycle_check = null;
      // found a candidate
      left.remove(current);
      dg(current, (string s) {
        auto n = s.lookup();
        left[n] = true;
      });
      finished[current] = true;
      if (left.length) current = left.keys[0];
      else current = null;
    }
  }
}
