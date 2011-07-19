/**
 * Authors: Steve Teale - steve.teale@britseyeview.com
 *
 * Date: 2007/05/19
 * History: V0.1
 * License: Use freely for any purpose.
 */
module bevutils.eventlogger;
 
import std.stdio;
import std.c.windows.windows;
import std.string;

enum FMT_MSG : uint
{
    FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x00000100,
    FORMAT_MESSAGE_IGNORE_INSERTS  = 0x00000200,
    FORMAT_MESSAGE_FROM_STRING     = 0x00000400,
    FORMAT_MESSAGE_FROM_HMODULE    = 0x00000800,
    FORMAT_MESSAGE_FROM_SYSTEM     = 0x00001000,
    FORMAT_MESSAGE_ARGUMENT_ARRAY  = 0x00002000,
    FORMAT_MESSAGE_MAX_WIDTH_MASK  = 0x000000FF
}

alias PVOID PSID;

extern (Windows)
{
    HANDLE RegisterEventSourceA(LPCTSTR lpUNCServerName, LPCTSTR lpSourceName);
    BOOL ReportEventA(HANDLE hEventLog,
            WORD wType,
            WORD wCategory,
            DWORD dwEventID,
            PSID lpUserSid,
            WORD wNumStrings,
            DWORD dwDataSize,
            LPCTSTR* lpStrings,
            LPVOID lpRawData);
    BOOL DeregisterEventSource(HANDLE hEventLog);
    UINT GetSystemDirectoryA(LPTSTR lpBuffer, UINT uSize);
}

/**
 * A class to write entries to the Windows system event log.
 */
class EventLogger
{
   enum : ushort
   {
      EVENTLOG_SUCCESS                = 0X0000,
      EVENTLOG_ERROR_TYPE             = 0x0001,
      EVENTLOG_WARNING_TYPE           = 0x0002,
      EVENTLOG_INFORMATION_TYPE       = 0x0004,
      EVENTLOG_AUDIT_SUCCESS          = 0x0008,
      EVENTLOG_AUDIT_FAILURE          = 0x0010
   }

   char* _appName;
   HANDLE _eventSource;

   /*****************************************************************************************
    * This is an explicit constructor to set a specific event source.
    *
    * To log an event, the event source must exist in the registry, and so the constructor
    * by default will ensure that an appropriate registry entry exists. For this to work,
    * the message file gpmf.dll must be present in the SYSTEM32 directory.
    *
    * Params:
    *    appName = The event source name
    *    forceRegEntry = Whether to attempt to create the event source registry entry.
    *                    If the entry already exists, no action is taken.
    */
   this(char[] appName, bool forceRegEntry = true) /// A comment
   {
      _appName = toStringz(appName);
      _eventSource = RegisterEventSourceA(null, _appName);
      if (forceRegEntry)
         createRegistryEntry(appName);
   }

   /*****************************************************************************************
    * A general-purpose constructor to set the event source from the module file name.
    *
    * To log an event, the event source must exist in the registry, and so the constructor
    * by default will ensure that an appropriate registry entry exists. For this to work,
    * the message file gpmf.dll must be present in the SYSTEM32 directory.
    *
    * Params:
    *    forceRegEntry = Whether to attempt to create the event source registry entry.
    *                    If the entry already exists, no action is taken.
    */
   this(bool forceRegEntry = true)
   {
      char[260] mfn;
      GetModuleFileNameA(null, mfn.ptr, 260);
      char[] smfn = std.string.toString(mfn.ptr);
      int lbs = rfind(smfn, '\\');
      int period = rfind(smfn, '.');
      smfn = smfn[lbs+1 .. period];
      this(smfn, forceRegEntry);
   }

   ~this()
   {
      DeregisterEventSource(_eventSource);
   }

   /*****************************************************************************************
    * Creates an event-source entry in HKLM\SOFTWARE\CurrentControlSet\Services\Eventlog.
    *
    * To log an event properly, an event source must exist in the registry. This method
    * creates an entry pointing to %SYSTEM32%\gpmf.dll, which is a very basic message file
    * with the usual range of severity entries, and with a single %1 replacement token.
    *
    * Params:
    *    appName = The event source name
    */
   public static bool createRegistryEntry(char[] appName)
   {
      char[260] sd;
      GetSystemDirectoryA(sd.ptr, 260);
      char[]sds = std.string.toString(sd.ptr);
      char[] val = sds ~ "\\gpmf.dll\0";
      uint ts = 7;
      char *path = toStringz("SYSTEM\\CurrentControlSet\\Services\\Eventlog\\Application\\" ~ appName);
      HKEY key = null;
      uint dispos;
      bool rv = false;
      try
      {
         do {
            if (RegCreateKeyExA(HKEY_LOCAL_MACHINE, path, 0, null, 0, KEY_SET_VALUE, null, &key, &dispos)
                     != ERROR_SUCCESS)
            {
               // It's already there - we presume it has suitable values
               rv = true;
               break;
            }
            if (dispos == REG_CREATED_NEW_KEY)
            {
               if (RegSetValueExA(key, toStringz("TypesSupported"), 0u, cast(uint) REG_DWORD, cast(ubyte*) &ts, 4)
                           != ERROR_SUCCESS)
                  break;
               if (RegSetValueExA(key, toStringz("EventMessageFile"), 0u, cast(uint) REG_SZ, cast(ubyte*) &val[0], val.length)
                           != ERROR_SUCCESS)
                  break;
            }
            rv = true;
         } while (0);
      }
      finally
      {
         if (key != null)
            RegCloseKey(key);
      }
      return rv;
   }

   /*****************************************************************************************
    * Log a message to the system event log.
    *
    * This is a general for where you specify the message and the severity.  For severity,
    * use the enum values EVENTLOG_SUCCESS (no message prefix), EVENTLOG_INFO_TYPE
    * (Info: message prefix), EVENTLOG_WARNING_TYPE (Warning: message prefix),
    * EVENTLOG_ERROR_TYPE (Error: message prefix).
    *
    * Params:
    *    msg = The message to be logged.
    *    severity = Enum values as above.
    */
   public void logEvent(char[] msg, ushort severity)
   {
      char*[1] msgList;
      static int xlate[5] = [ 0,3,2,0,1 ];
      uint t = xlate[severity];
      uint code = (t << 30) | t;
      msgList[0] = std.string.toStringz(msg);

      if (_eventSource != null)
      {
          ReportEventA(_eventSource,          // handle of event source
                      severity,               // event type
                      0,                      // event category
                      code,                   // event ID
                      null,                   // current user's SID
                      1,                      // number of message strings
                      0,                      // no bytes of raw data
                      cast(char**) msgList,   // array of error strings
                      null);                  // no raw data

      }
   }

   /*****************************************************************************************
    * Log a general purpose message to the system event log.
    *
    * This is a specific method implemented as
    * logMessage(char[] msg) { logEvent(msg, EVENTLOG_SUCCESS); }
    *
    * Params:
    *    msg = The message to be logged.
    */
   public void logMessage(char[] msg) { logEvent(msg, EVENTLOG_SUCCESS); }

   /*****************************************************************************************
    * Log an information message to the system event log.
    *
    * This is a specific method implemented as
    * logMessage(char[] msg) { logEvent(msg, EVENTLOG_INFORMATION_TYPE); }
    *
    * Params:
    *    msg = The message to be logged.
    */
   public void logInfo(char[] msg) { logEvent(msg, EVENTLOG_INFORMATION_TYPE); }

      /*****************************************************************************************
    * Log a warning message to the system event log.
    *
    * This is a specific method implemented as
    * logMessage(char[] msg) { logEvent(msg, EVENTLOG_WARNING_TYPE); }
    *
    * Params:
    *    msg = The message to be logged.
    */
   public void logWarning(char[] msg) { logEvent(msg, EVENTLOG_WARNING_TYPE); }

   /*****************************************************************************************
    * Log an error message to the system event log.
    *
    * This is a specific method implemented as
    * logMessage(char[] msg) { logEvent(msg, EVENTLOG_ERROR_TYPE); }
    *
    * Params:
    *    msg = The message to be logged.
    */
   public void logError(char[] msg) { logEvent(msg, EVENTLOG_ERROR_TYPE); }

   /*****************************************************************************************
    * Get an error message from Windows given a Win32 error number.
    *
    * Params:
    *    errnum = The error number for which to recover a message.
    */
   public char[] getWindowsErrorText(uint errnum)
   {
      DWORD dwRet;
      char* lpszTemp = null;
      char[512] buf;
      char[] rv;

      dwRet = FormatMessageA( FMT_MSG.FORMAT_MESSAGE_ALLOCATE_BUFFER | FMT_MSG.FORMAT_MESSAGE_FROM_SYSTEM |FMT_MSG.FORMAT_MESSAGE_ARGUMENT_ARRAY,
                       null,
                       errnum,
                       0,
                       cast(char *) &lpszTemp,
                       0,
                       null);

      // supplied buffer is not long enough
      if ( !dwRet || 512 < dwRet+14)
      {
         rv = "";
      }
      else
      {
         rv = std.string.toString(lpszTemp);
      }  
      if (lpszTemp)
         LocalFree(cast(HLOCAL) lpszTemp );
      return rv;
   }

   /*****************************************************************************************
    * Log an error message incorporating Win32 error details to the system event log.
    *
    * This method allows you to provide the error number from GetLastError().
    *
    * Params:
    *    msg = The message to be prepended to the error details.
    *    errnum = The return value from GetLastError().
    */
   public void logWindowsError(char[] msg, uint errnum)
   {
      DWORD dwRet;
      char* lpszTemp = null;
      char[512] buf;
      char[] rv;
      char[] delim;

      dwRet = FormatMessageA( FMT_MSG.FORMAT_MESSAGE_ALLOCATE_BUFFER | FMT_MSG.FORMAT_MESSAGE_FROM_SYSTEM |FMT_MSG.FORMAT_MESSAGE_ARGUMENT_ARRAY,
                       null,
                       errnum,
                       0,
                       cast(char *) &lpszTemp,
                       0,
                       null);

      // supplied buffer is not long enough
      if ( !dwRet || 512 < dwRet+14)
      {
         rv = "";
         delim = "";
      }
      else
      {
         rv = std.string.toString(lpszTemp);
         delim = "\n";
      }  
      if (lpszTemp)
         LocalFree(cast(HLOCAL) lpszTemp );
      char[] serr = (msg == "")?
                     "(" ~ std.string.toString(errnum) ~ "): " ~ rv:
                     msg ~ "\n(" ~ std.string.toString(errnum) ~ "): " ~ rv;
      logEvent(serr, EVENTLOG_ERROR_TYPE);
   }

   /*****************************************************************************************
    * Log an error message incorporating Win32 error details to the system event log.
    *
    * This method calls GetLastError() to get the error number for you.
    *
    * Params:
    *    msg = The message to be prepended to the error details.
    *    errnum = The return value from GetLastError().
    */
   public void logWindowsError(char[] msg)
   {
      uint errnum = GetLastError();
      logWindowsError(msg, errnum);
   }
}
/*
void main(char[][] args)
{
   EventLogger el = new EventLogger();
   el.logMessage("This is steve's event");
   el.logInfo("This is steve's event");
   el.logWarning("This is steve's event");
   el.logError("This is steve's event");
   el.logWindowsError("Whatever", 10u);
   writefln(el.getWindowsErrorText(10));
   delete el;
}
*/
