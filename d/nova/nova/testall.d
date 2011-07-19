module testall;

//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////

// import external unittests
import nova.math.unittests.matrix;

import nova.ds.intrusive.unittests.doublelink;
import nova.ds.intrusive.unittests.doublelinkedlist;
import nova.ds.intrusive.unittests.redblacktree;
import nova.ds.intrusive.unittests.singlelinkedlist;
import nova.ds.intrusive.unittests.singlelinkedring;

// import the rest
import nova.all;

// import tango loggers
import tango.util.log.Log;
import tango.util.log.ConsoleAppender;

unittest
{
  Log.getRootLogger().addAppender(new ConsoleAppender());
  Log.getLogger("test").info("Hello Tango!");
}

//////////////////////////////////////////////////////////////////////////
/**

  unittest stub

*/////////////////////////////////////////////////////////////////////////
int main()
{
  return 0;
}
