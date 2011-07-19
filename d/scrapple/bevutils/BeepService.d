/**
 * Authors: Steve Teale - steve.teale@britseyeview.com
 *
 * Date: 2007/05/19
 * History: V0.1
 * License: Use freely for any purpose.
 *
 * This is an example of a service built on class ServiceImplementation.
 */
module bevutils.beepservice;

import std.stdio;
import std.c.windows.windows;
import bevutils.log4d;
import bevutils.eventlogger;
import bevutils.propertyfile;
import bevutils.servicebase;
import bevutils.serviceimplementation;

extern (Windows)
{
   BOOL Beep(DWORD dwFreq, DWORD dwDuration);
}
/++
 + To implement the service you must implement one or more classes derived from interface WorkerThread
 + and a class derived from ServiceImplementation.
 + You must also provide a stereotyped version of main().
 +
 + ------------------------------------------------------
class BeepWorker : WorkerThread
{
   Log4D _log;
   ServiceImplementation _si;
   EventLogger _elog;
   PropertyFile _props;
   uint _frequency;
   uint _duration;

   this(uint freq, uint dur)
   {
      _frequency = freq;
      _duration = dur;
   }

   void setLog(Log4D log) { _log = log; }
   void setHost(ServiceImplementation si) { _si = si; }
   void setProperties(PropertyFile pf) { _props = pf; }
   void setEventLogger(EventLogger el) { _elog = el; }
   char[] threadName() { return "Beeper"; }

   int threadProc()
   {
      for (;;)
      {
         Beep(_frequency, _duration);
		   _log.logMessage("INFO", "Beeped");
         for (int i = 0; i < 20; i++)
         {
            Sleep(500);
            if (_si.StopFlag)
				{
		         _log.logMessage("INFO", "Beeper stopping");
               return 0;
			   }
         }
      }
		return 0;
   }
}

class BeepService : ServiceImplementation
{
   this()
   {
      super("Beeper", "BPR");
   }

   public WorkerThread getThreadImpl(int n)
   {
	   if (n == 0)
			return new BeepWorker(200, 500);
      else
		   return new BeepWorker(650, 200);
   }
}

void main(char[][] args)
{
    try
    {
        ServiceImplementation si = new BeepService();
        ServiceBase.implementMain(args);
    }
    catch (Exception ex)
    {
        writefln(ex.toString());
    }
}
 + ------------------------------------------------------
 +/
class BeepWorker : WorkerThread
{
   Log4D _log;
   ServiceImplementation _si;
   EventLogger _elog;
   PropertyFile _props;
   uint _frequency;
   uint _duration;

   this(uint freq, uint dur)
   {
      _frequency = freq;
      _duration = dur;
   }

   void setLog(Log4D log) { _log = log; }
   void setHost(ServiceImplementation si) { _si = si; }
   void setProperties(PropertyFile pf) { _props = pf; }
   void setEventLogger(EventLogger el) { _elog = el; }
   char[] threadName() { return "Beeper"; }

   int threadProc()
   {
      for (;;)
      {
         Beep(_frequency, _duration);
		   _log.logMessage("INFO", "Beeped");
         for (int i = 0; i < 20; i++)
         {
            Sleep(500);
            if (_si.StopFlag)
				{
		         _log.logMessage("INFO", "Beeper stopping");
               return 0;
			   }
         }
      }
		return 0;
   }
}

/**
 * You must also override class ServiceImplementation to provide appropriate
 * information to its constructor, and to provide an implementation for
 * getThreadImpl.
 */
class BeepService : ServiceImplementation
{
   this()
   {
      super("Beeper", "BPR");
   }

   public WorkerThread getThreadImpl(int n)
   {
	   if (n == 0)
			return new BeepWorker(200, 500);
      else
		   return new BeepWorker(650, 200);
   }
}

/**
 * Main is completely stereotyped.
 *
 * The properties file for this service should be as follows:
 * ---------------------------------------------------------
 * <?xml version="1.0" ?>
 * <Properties layout="1">
 *   <logpath type="string">d:\logs</logpath>
 *    <numlogs type="int">10</numlogs>
 *    <maxlogsize type="int">1000000</maxlogsize>
 *    <threads type="int">2</threads>
 * </Properties>
 * ---------------------------------------------------------
 * You could of course put the beep frequencies and durations
 * in an int[] in the properties file.
 */
void main(char[][] args)
{
    try
    {
        ServiceImplementation si = new BeepService();
        ServiceBase.implementMain(args);
    }
    catch (Exception ex)
    {
        writefln(ex.toString());
    }
}
