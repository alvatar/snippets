/***************************************************************
Copyright (c) Steve Teale 2007
This program is free software; you can use it for any purpose
subject to the following conditions.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.
****************************************************************/
module bevutils.log4d;

import std.stdio;
import std.file;
import std.date;
import std.stream;
import std.regexp;
import std.string;

/**
 * General purpose logging class with file count and size limits and rollover.
 *
 * The name implies no similarity to the workings of log4j
 */
class Log4D
{
private:
   const char[] _rex = r"2[0-9]{3}[0-1][0-9][0-3][0-9][0-2][0-9][0-5][0-9][0-5][0-9][0-9]{3}\.log";
   uint _maxsize;
   uint _maxfiles;
   char[] _path;
   char[] _prefix;
   char[] _current;
   int _numlogs;
   bool _ok, _isopen;
   int _cycle;
   File _log;

   private char[] makeLogName()
   {
      d_time t = UTCtoLocalTime(getUTCtime());
      Date d;
      d.fromTicks(t);
      char[] ds = std.string.format("%d%02d%02d%02d%02d%02d%03d", d.year, d.month, d.day, d.hour, d.minute, d.second, d.ms);
      return std.string.format("%s\\%s%s.log", _path, _prefix, ds);
   }

   private void findCurrent()
   {
      char[][] list = listthisdir(_path, RegExp(_prefix ~ _rex));
      _numlogs = list.length;
      if (_numlogs)
      {
         list = list.sort;
         _current = list[_numlogs-1];
      }
      else
      {
         _current = makeLogName();
         _numlogs = 1;
      }
   }


   private bool rollOver()
   {
      if (_isopen)
      {
         _log.close();
         _isopen = false;
      }
      ulong sz = getSize(_current);
      if (sz < _maxsize)
      {
         _log = new File(_current, std.stream.FileMode.Append);
         _isopen = true;
         return false;
      }
      char[] newname = makeLogName();
      if (newname == _current)
      {
         // We've filled a log file in less than a millisecond - increase _maxsize
         // but in this case, just lie and continue to grow the existing file
         return true;
      }
      _current = newname;
      // Create empty file
      std.file.write(_current, cast(void[])"");
      _numlogs++;

      if (_numlogs > _maxfiles)
      {
         int n = _numlogs - _maxfiles;
         char[][] list = listthisdir(_path, RegExp(_prefix ~ _rex));
         if (list.length)
         {
            list = list.sort;
            for (int i = 0; i < n; i++)
            {
               std.file.remove(list[i]);
            }
            _numlogs = _maxfiles;
         }
         else
            _numlogs = 0;
      }

      return true;
   }

public:
   /**
    * At the moment there is only one logging style, but please feel free to roll your own.
    */
   enum Log4DStyle
   {
      STD
   }

   /**
    * Single constructor
    *
    * Log files are named with a prefix to indicate their host application, and a timestamp to
    * the millisecond.
    *
    * Params:
    *    path = Path to the directory where log files are to be kept
    *    prefix = Distinguishing prefix for the log files - usually an abbreviation of the host application name.
    *    style = One of possibly several styles mentioned in enum Log4DStyle - only one now.
    *    maxfiles = Maximum number of log files to retain at any time.
    *    maxsize = Size at which to rollover and create a new log file.
    */
   this(char[] path, char[] prefix, Log4DStyle style = Log4DStyle.STD, int maxfiles = 0, int maxsize = 0)
   {
      _ok = false;
      _isopen = false;
      _cycle = 0;
      _log = null;
      _maxsize = (maxsize == 0)? 0x400000: maxsize;
      if (_maxsize < 0x100000)
         _maxsize = 0x100000;       // Get a bigger hard drive ;=)
      _maxfiles = (maxfiles == 0)? 5: maxfiles;
      int l = path.length;
      if (path[l-1] == '\\')
      path.length = l-1;
      _path = path;
      _prefix = prefix;
      _current = "";
      _numlogs = 0;
      try
      {
         findCurrent();
         _log = new File(_current, std.stream.FileMode.Append);
         _isopen = true;
         _ok = true;
      }
      catch (Exception) {}
   }

   ~this() { _log.close(); }

   /**
    * Sentinel to check that the logger was created OK.
    */
   public bool OK() { return _ok; }


   /**
    * The method to log messages.
    *
    * Override this to create you own message formats
    *
    * This version creates log entries like:<br>
    * "[YYYY-MM-DD HH:MM:SS:mmm] INFO: The message"
    *
    * Params:
    *    msgType = A string to label the message after the timestamp - e.g. INFO
    *    msg = The text to be logged
    */
   public synchronized void logMessage(char[] msgType, char[] msg)
   {
      // Check occasionally to see if the log file is over its stipulated size,
      // and start a new one if it is.
      if (_cycle > 20)
      {
         if (rollOver())
         {
            _log = new File(_current, std.stream.FileMode.Append);
            _isopen = true;
            _cycle = 0;
         }
      }
      else
         _cycle++;

      d_time t = UTCtoLocalTime(getUTCtime());
      Date d;
      d.fromTicks(t);
      char[] ds = std.string.format("[%d-%02d-%02d %02d:%02d:%02d:%03d] %s: %s",
                           d.year, d.month, d.day, d.hour, d.minute, d.second, d.ms, msgType, msg);
         
      _log.writeLine(ds);
   }
}

/+
void main(char[][] args)
{
   Log4D log = new Log4D(r"d:\aaa", "XYZ");
   for (int i = 0; i < 2000000; i++)
      log.logMessage("INFO", "A somewhat long and pointless message simply intended to fill up reams of log file and otherwise to achieve nothing.");
}
+/

