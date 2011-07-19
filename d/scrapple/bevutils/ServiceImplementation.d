/**
 * Authors: Steve Teale - steve.teale@britseyeview.com
 *
 * Date: 2007/05/19
 * History: V0.1
 * License: Use freely for any purpose.
 */
module bevutils.serviceimplementation;

import std.stdio;
import std.string;
import std.c.windows.windows;
import std.thread;
import bevutils.servicebase;
import bevutils.eventlogger;
import bevutils.log4d;
import bevutils.propertyfile;

extern (Windows)
{
   BOOL Beep(DWORD dwFreq, DWORD dwDuration);
}

interface WorkerThread
{
   int threadProc();
   void setLog(Log4D log);
   char[] threadName();
   void setHost(ServiceImplementation si);
   void setProperties(PropertyFile pf);
   void setEventLogger(EventLogger el);
}
/**
 * A class derived fromn ServiceBase to provide a more comprehensive implementation
 * of a service, including:
 *
 * - Logging to file with rollover and limitation of file accumulation.<br>
 * - Reading of an XML properties file on service start.<br>
 * - Utilization of a number of threads for the actual implementation.
 */
class ServiceImplementation : ServiceBase
{
protected:
   PropertyFile _props;
   char[] _abbrev;
   Log4D _log;
   bool _logging;
   Thread[] _threads;
   bool _stopFlag;
	int _loglevel;

   /**
    * Single constructor to provide the service name to the base class, and note
    * the abbreviation for use as log file prefix.
    */
   this(char[] serviceName, char[] abbrev)
   {
      super(serviceName);
      _abbrev = abbrev;
   }

   void getAppProperties()
   {
      char[512] szPath;
      int n = GetModuleFileNameA(null, szPath.ptr, 512);
      if (!n)
      {
         throw new Exception("Unable to get executable path for properties.");
      }
      char[] s = szPath[0 .. n];
      _props = new PropertyFile(s ~ ".config");
   }

   public void onServiceStart(char[][] args)
   {
      // Get the properties from the application properties file.  This is presumed to be called
      // appname.exe.config, and to be in the same directory as the executable.
      getAppProperties();

      // Now create the service's log file if logging is specified by the existence of a logpath
      // property.
      _logging = false;
      if (_props("logpath"))
      {
         char[] logpath = _props.getString("logpath");
         int numlogs = _props.getInt("numlogs");
         int maxlogsize = _props.getInt("maxlogsize");
         _log = new Log4D(logpath, _abbrev, Log4D.Log4DStyle.STD, numlogs, maxlogsize);
         _logging = true;
         _log.logMessage("INFO", "Logging to " ~ logpath);
      }
		_loglevel = 0;
		if (_props("verbosity"))
		   _loglevel = _props.getInt("loglevel");
      int threads = 1;
      if (_props("threads"))
      {
         threads = _props.getInt("threads");
      }
      _threads.length = threads;
      int delegate() dg;
      for (int i = 0; i < threads; i++)
      {
         WorkerThread wt = getThreadImpl(i);
         if (_logging)
            wt.setLog(_log);
         wt.setHost(this);
         wt.setProperties(_props);
         wt.setEventLogger(_elog);
         dg = &wt.threadProc;
         _threads[i] = new Thread(dg);
         _threads[i].start();
         if (_logging)
            _log.logMessage("INFO", wt.threadName() ~ " started as thread " ~ std.string.toString(i));
      }
   }

   void onServiceStop()
   {
      _stopFlag = true;
      for (;;)
      {
         int n = 0;
         for (int i = 0; i <_threads.length; i++)
         {
            if (_threads[i].getState() != Thread.TS.TERMINATED)
               n++;
         }
         if (!n)
            break;
         ReportStatusToSCMgr(SERVICE_STATES.SERVICE_STOP_PENDING, 0, 0);
         Sleep(500);
      }
   }
public:

   /**
    * Override this method to create an instance of an object implementing WorkerThread
    * FOR THREADS 0 - n-1.
    */
   abstract WorkerThread getThreadImpl(int n)
   {
      return null;
   }

   /**
    * Your WorkerThread implementation must check this flag to determine when the thread should terminate.
    */
   bool StopFlag() { return _stopFlag; }

   void logAlways(char[] category, char[] msg)
   {
   	_log.logMessage(category, msg);
   }

	void logError(char[] msg)
	{
	   _log.logMessage("ERROR", msg);
	}

	void logWarning(char[] msg)
	{
	   if (_loglevel >= 1)
		   _log.logMessage("WARNING", msg);
	}

	void logInfo(char[] msg)
	{
	   if (_loglevel >= 2)
		   _log.logMessage("INFO", msg);
	}
}
