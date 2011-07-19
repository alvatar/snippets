 template _SyncObj(alias A) { Object obj; static this() { obj = new Object; } } template SyncObj(alias A) { alias _SyncObj!(A).obj SyncObj; }
