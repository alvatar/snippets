/**
 * Authors: Steve Teale - steve.teale@britseyeview.com
 *
 * Date: 2007/05/19
 * History: V0.1
 * License: Use freely for any purpose.
 */
module bevutils.servicebase;

import std.stdio;
import std.c.windows.windows;
import std.string;
import bevutils.eventlogger;

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

const int SERVICE_WIN32_OWN_PROCESS = 0x00000010;
enum ServiceControlManagerType : int
{
    SC_MANAGER_CONNECT = 0x1,
    SC_MANAGER_CREATE_SERVICE = 0x2,
    SC_MANAGER_ENUMERATE_SERVICE = 0x4,
    SC_MANAGER_LOCK = 0x8,
    SC_MANAGER_QUERY_LOCK_STATUS = 0x10,
    SC_MANAGER_MODIFY_BOOT_CONFIG = 0x20,
    SC_MANAGER_ALL_ACCESS = 
        SC_MANAGER_CONNECT |
        SC_MANAGER_CREATE_SERVICE |
        SC_MANAGER_ENUMERATE_SERVICE |
        SC_MANAGER_LOCK |
        SC_MANAGER_QUERY_LOCK_STATUS |
        SC_MANAGER_MODIFY_BOOT_CONFIG
}

enum ACCESS_TYPE : int
{
    SERVICE_QUERY_CONFIG = 0x1,
    SERVICE_CHANGE_CONFIG = 0x2,
    SERVICE_QUERY_STATUS = 0x4,
    SERVICE_ENUMERATE_DEPENDENTS = 0x8,
    SERVICE_START = 0x10,
    SERVICE_STOP = 0x20,
    SERVICE_PAUSE_CONTINUE = 0x40,
    SERVICE_INTERROGATE = 0x80,
    SERVICE_USER_DEFINED_CONTROL = 0x100,
    SERVICE_ALL_ACCESS = 
        SERVICE_QUERY_CONFIG |
        SERVICE_CHANGE_CONFIG |
        SERVICE_QUERY_STATUS |
        SERVICE_ENUMERATE_DEPENDENTS |
        SERVICE_START |
        SERVICE_STOP |
        SERVICE_PAUSE_CONTINUE |
        SERVICE_INTERROGATE |
        SERVICE_USER_DEFINED_CONTROL,
        DELETE = 0x00010000
}

enum SERVICE_COMMANDS : int
{
    SERVICE_CONTROL_STOP                   = 1,
    SERVICE_CONTROL_PAUSE                  = 2,
    SERVICE_CONTROL_CONTINUE               = 3,
    SERVICE_CONTROL_INTERROGATE            = 4,
    SERVICE_CONTROL_SHUTDOWN               = 5,
    SERVICE_CONTROL_PARAMCHANGE            = 6,
    SERVICE_CONTROL_NETBINDADD             = 7,
    SERVICE_CONTROL_NETBINDREMOVE          = 8,
    SERVICE_CONTROL_NETBINDENABLE          = 9,
    SERVICE_CONTROL_NETBINDDISABLE         = 10,
    SERVICE_CONTROL_DEVICEEVENT            = 11,
    SERVICE_CONTROL_HARDWAREPROFILECHANGE  = 12,
    SERVICE_CONTROL_POWEREVENT             = 13,
    SERVICE_CONTROL_SESSIONCHANGE          = 14
}

enum SERVICE_STATES : uint
{
    SERVICE_STOPPED = 1,
    SERVICE_START_PENDING,
    SERVICE_STOP_PENDING,
    SERVICE_RUNNING,
    SERVICE_CONTINUE_PENDING,
    SERVICE_PAUSE_PENDING,
    SERVICE_PAUSED
}

enum SERVICE_START_TYPE : uint
{
    SERVICE_BOOT_START             = 0x00000000,
    SERVICE_SYSTEM_START           = 0x00000001,
    SERVICE_AUTO_START             = 0x00000002,
    SERVICE_DEMAND_START           = 0x00000003,
    SERVICE_DISABLED               = 0x00000004
}

enum SERVICE_CONTROL_TYPE : uint
{
    SERVICE_ERROR_IGNORE           = 0x00000000,
    SERVICE_ERROR_NORMAL           = 0x00000001,
    SERVICE_ERROR_SEVERE           = 0x00000002,
    SERVICE_ERROR_CRITICAL         = 0x00000003
}
enum ACCEPT_COMMANDS : uint
{
    SERVICE_ACCEPT_STOP                    = 0x00000001,
    SERVICE_ACCEPT_PAUSE_CONTINUE          = 0x00000002,
    SERVICE_ACCEPT_SHUTDOWN                = 0x00000004,
    SERVICE_ACCEPT_PARAMCHANGE             = 0x00000008,
    SERVICE_ACCEPT_NETBINDCHANGE           = 0x00000010,
    SERVICE_ACCEPT_HARDWAREPROFILECHANGE   = 0x00000020,
    SERVICE_ACCEPT_POWEREVENT              = 0x00000040,
    SERVICE_ACCEPT_SESSIONCHANGE           = 0x00000080
}

enum EVENTLOG_TYPES : int
{
    EVENTLOG_SUCCESS                = 0x0000,
    EVENTLOG_ERROR_TYPE             = 0x0001,
    EVENTLOG_WARNING_TYPE           = 0x0002,
    EVENTLOG_INFORMATION_TYPE       = 0x0004,
    EVENTLOG_AUDIT_SUCCESS          = 0x0008,
    EVENTLOG_AUDIT_FAILURE          = 0x0010
}

alias void* SC_HANDLE;
alias void* SERVICE_STATUS_HANDLE;
alias PVOID PSID; 

struct SERVICE_TABLE_ENTRY
{
    char* ServiceName;
    void* ServiceMain;
}
struct SERVICE_STATUS
{
    uint dwServiceType;
    uint dwCurrentState;
    uint dwControlsAccepted;
    uint dwWin32ExitCode;
    uint dwServiceSpecificExitCode;
    uint dwCheckPoint;
    uint dwWaitHint;
}

extern (Windows)
{
    SC_HANDLE OpenServiceA(
            SC_HANDLE hSCManager, 
            char* lpServiceName, 
            ACCESS_TYPE dwDesiredAccess);
    SC_HANDLE OpenSCManagerA(
			char* lpMachineName, char* lpDatabaseName,
			ServiceControlManagerType dwDesiredAccess);
    BOOL CloseServiceHandle(
			SC_HANDLE hSCObject);
    BOOL DeleteService(SC_HANDLE hSCObject);
    BOOL StartServiceCtrlDispatcherA(SERVICE_TABLE_ENTRY*);
    SERVICE_STATUS_HANDLE RegisterServiceCtrlHandlerA(char*, void*);
    BOOL SetServiceStatus(SERVICE_STATUS_HANDLE, SERVICE_STATUS*);
    SC_HANDLE CreateServiceA(
            SC_HANDLE hSCManager,
            LPCTSTR lpServiceName,
            LPCTSTR lpDisplayName,
            DWORD dwDesiredAccess,
            DWORD dwServiceType,
            DWORD dwStartType,
            DWORD dwErrorControl,
            LPCTSTR lpBinaryPathName,
            LPCTSTR lpLoadOrderGroup,
            LPDWORD lpdwTagId,
            LPCTSTR lpDependencies,
            LPCTSTR lpServiceStartName,
            LPCTSTR lpPassword);
    BOOL ControlService(
            SC_HANDLE hService,
            DWORD dwControl,
            SERVICE_STATUS* lpServiceStatus);
    BOOL QueryServiceStatus(
            SC_HANDLE hService,
            SERVICE_STATUS* lpServiceStatus);
    HANDLE CreateEventA(LPSECURITY_ATTRIBUTES lpEventAttributes,
            BOOL bManualReset,
            BOOL bInitialState,
            LPCTSTR lpName);
    BOOL SetEvent(HANDLE hEvent);
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
    BOOL Beep(DWORD dwFreq, DWORD dwDuration);


}

/++
 + A class to provide the internals of a windows service, including installation and removal.
 +
 + The class has few methods that you would call from a host program.
 + All detailed implementation must be provided by a derived class.
 +
 + An example of its use to create a service executable is presented below.
 +
 + To install the service run derived.exe -i, to remove it run derived.exe -r.
 +
 + -----------------------------------------------------
// Derive a class from ServiceBase implementing the abstract methods.
class NullService : ServiceBase
{
    this()
    {
        super("Dummy", false);
    }

    public void onServiceStart(char[][] args)
    {
        ServiceBase.EventLog.logMessage("NullService onServiceStart was called");
    }

    public void onServiceStop()
    {
        ServiceBase.EventLog.logMessage("NullService onServiceStop was called");
    }
}

// Create an object of the derived class, then call the base class implementMain method.
void main(char[][] args)
{
    try
    {
        ServiceBase sb = new NullService();
        ServiceBase.implementMain(args);
    }
    catch (Exception ex)
    {
        writefln(ex.toString());
    }
}
 + -----------------------------------------------------
 +/
class ServiceBase
{
protected:
    static char[] _serviceName;
    static SERVICE_TABLE_ENTRY[2] _sta;
    static bool _debug;
    static SERVICE_STATUS _ss;
    static SERVICE_STATUS_HANDLE _ssh;
    static char[][] _args;
    static int _inited;
    static ServiceBase _self;
    static HANDLE _stopEvent;
    static EventLogger _elog;

    this(char[] serviceName)
    {
        _ssh = null;
        if (_inited == 0)
        {
            //_debug = dBG;
            _serviceName = serviceName;
            _sta[0].ServiceName = toStringz(_serviceName);
            _sta[0].ServiceMain = cast(void*) &service_main;
            _sta[1].ServiceName = null;
            _sta[1].ServiceMain = null;
            _elog = new EventLogger();
            _self = this;
        }
        _inited = 1;
    }

    static void StartService()
    {
        if (!StartServiceCtrlDispatcherA(cast(SERVICE_TABLE_ENTRY *) &_sta[0]))
        {
            _elog.logWindowsError("StartServiceCtrlDispatcherA failed.");
        }
    }

    void ServiceStart (char[][] args)
    {
        try
        {
            if (!ReportStatusToSCMgr(SERVICE_STATES.SERVICE_START_PENDING,  // service state
                                     0,                                     // exit code
                                     3000))                                 // wait hint
                throw new Exception("Failed to report status to SCM");
            _stopEvent = CreateEventA(
                                 null,      // no security attributes
                                 1,         // manual reset event
                                 0,         // not-signalled
                                 null);     // no name
            if (_stopEvent == null)
                throw new Exception("Failed to create stop event");
            if (!ReportStatusToSCMgr(SERVICE_STATES.SERVICE_START_PENDING,
                                     0,
                                     3000))
                throw new Exception("Failed to report status to SCM");
            onServiceStart(args);
            if (!ReportStatusToSCMgr(SERVICE_STATES.SERVICE_RUNNING,       // service state
                                     0,                                    // exit code
                                     0))                                   // wait hint
                throw new Exception("Failed to report status to SCM");
            _elog.logInfo(_serviceName ~ " started.");
            WaitForSingleObject(_stopEvent, INFINITE);
           _elog.logInfo(_serviceName ~ " stopped.");
            ReportStatusToSCMgr(SERVICE_STATES.SERVICE_STOPPED, 0, 0);
        }
        finally {}
    }

    void ServicePause()
    {
      onServicePause();
      ReportStatusToSCMgr(SERVICE_STATES.SERVICE_PAUSED, 0, 0);
      _elog.logInfo(_serviceName ~ " paused.");
    }

    void ServiceContinue()
    {
      onServiceContinue();
      ReportStatusToSCMgr(SERVICE_STATES.SERVICE_RUNNING, 0, 0);
      _elog.logInfo(_serviceName ~ " resumed.");
    }

    void ServiceStop()
    {
        onServiceStop();
        if (_stopEvent)
            SetEvent(_stopEvent);
    }

    public abstract void onServiceStart(char[][] args)
    {
    }

    public abstract void onServiceStop()
    {
        // Override this method to shut down the worker thread(s) of your service
    }

    public void onServicePause()
    {
        // Override this method to pause the worker thread(s) of your service
    }

    public void onServiceContinue()
    {
        // Override this method to resume the worker thread(s) of your service
    }

    static void InstallService()
    {
        SC_HANDLE   schService;
        SC_HANDLE   schSCManager;

        char[512] szPath;
        if (GetModuleFileNameA(null, szPath.ptr, 512) == 0 )
        {
            writefln("Unable to get executable path - service installation failed");
            return;
        }

        schSCManager = OpenSCManagerA(
                                null,                       // machine (NULL == local)
                                null,                       // database (NULL == default)
                                ServiceControlManagerType.SC_MANAGER_ALL_ACCESS  // access required
                                );
        if (schSCManager)
        {
            schService = CreateServiceA(
                                schSCManager,               // SCManager database
                                CName(),                    // name of service
                                CName(),                    // name to display
                                ACCESS_TYPE.SERVICE_QUERY_STATUS,         // desired access
                                SERVICE_WIN32_OWN_PROCESS,  // service type
                                SERVICE_START_TYPE.SERVICE_DEMAND_START,  // start type
                                SERVICE_CONTROL_TYPE.SERVICE_ERROR_NORMAL,// error control type
                                toStringz(szPath),                 // service's binary
                                null,                       // no load ordering group
                                null,                       // no tag identifier
                                null,                       // dependencies
                                null,                       // LocalSystem account
                                null);                      // no password

            if (schService)
            {
                writefln("%s installed", _serviceName);
                CloseServiceHandle(schService);
            }
            else
            {
                uint err = GetLastError();
                char[] errMsg = _elog.getWindowsErrorText(err);
                writefln("CreateService failed (%d) - %s", err, errMsg);
            }

            CloseServiceHandle(schSCManager);
        }
        else
        {
            uint err;
            char[] errMsg = _elog.getWindowsErrorText(err);
            writefln("OpenSCManager failed (%d) - %s", err, errMsg);
        }
    }

    static void RemoveService()
    {
        SC_HANDLE   schService;
        SC_HANDLE   schSCManager;

        schSCManager = OpenSCManagerA(
                               null,                   // machine (NULL == local)
                               null,                   // database (NULL == default)
                               ServiceControlManagerType.SC_MANAGER_CONNECT);      // access required
        if ( schSCManager )
        {
            schService = OpenServiceA(schSCManager, CName(),
                                        ACCESS_TYPE.DELETE | ACCESS_TYPE.SERVICE_STOP | ACCESS_TYPE.SERVICE_QUERY_STATUS);

            if (schService)
            {
                // try to stop the service
                if (ControlService(schService, SERVICE_COMMANDS.SERVICE_CONTROL_STOP, &_ss))
                {
                    writefln("Stopping %s.", _serviceName);
                    Sleep(1000);

                    while (QueryServiceStatus( schService, &_ss ))
                    {
                        if ( _ss.dwCurrentState == SERVICE_STATES.SERVICE_STOP_PENDING )
                        {
                            writef(".");
                            Sleep( 1000 );
                        }
                        else
                            break;
                    }

                    if (_ss.dwCurrentState == SERVICE_STATES.SERVICE_STOPPED)
                        writefln("\n%s stopped.", _serviceName);
                    else
                        writefln("\n%s failed to stop.\n", _serviceName);

                 }

                 // now remove the service
                 if (DeleteService(schService))
                     writefln("%s removed.", _serviceName);
                 else
                 {
                     uint err = GetLastError();
                     char[] errMsg = _elog.getWindowsErrorText(err);
                     writefln("DeleteService failed (%d) - %s", err, errMsg);
                 }

                 CloseServiceHandle(schService);
            }
            else
            {
                uint err = GetLastError();
                char[] errMsg = _elog.getWindowsErrorText(err);
                writefln("OpenService failed (%d) - %s", err, errMsg);
            }
            CloseServiceHandle(schSCManager);
        }
        else
        {
            uint err = GetLastError();
            char[] errMsg = _elog.getWindowsErrorText(err);
            writefln("OpenSCManager failed (%d) - %s", err, errMsg);
        }
    }

public:

    /**
     * Get the service name as a C style string.
     */
    static char *CName() { return toStringz(_serviceName); }

    /**
     * Get a reference to the services event logger.
     */
    static EventLogger EventLog() { return _elog; }

    /**
     * Get the service name as a D style string.
     */
    static char[] Name() { return _serviceName; }

    /**
     * This method should be called only by the SCM.
     */
    extern (Windows) static export void service_ctrl(uint dwCtrlCode)
    {
        // Handle the requested control code.
        switch (dwCtrlCode)
        {
            // Stop the service.
            //
            // SERVICE_STOP_PENDING should be reported before
            // setting the Stop Event - hServerStopEvent - in
            // ServiceStop().  This avoids a race condition
            // which may result in a 1053 - The Service did not respond...
            // error.
            case SERVICE_COMMANDS.SERVICE_CONTROL_STOP:
                ReportStatusToSCMgr(SERVICE_STATES.SERVICE_STOP_PENDING, 0, 0);
                _self.ServiceStop();
                return;
            case SERVICE_COMMANDS.SERVICE_CONTROL_PAUSE:
                ReportStatusToSCMgr(SERVICE_STATES.SERVICE_PAUSE_PENDING, 0, 0);
                _self.ServicePause();
                return;
            case SERVICE_COMMANDS.SERVICE_CONTROL_CONTINUE:
                ReportStatusToSCMgr(SERVICE_STATES.SERVICE_CONTINUE_PENDING, 0, 0);
                _self.ServiceContinue();
                return;

            // Update the service status.
            //
            case SERVICE_COMMANDS.SERVICE_CONTROL_INTERROGATE:
                break;

            // invalid control code
            //
            default:
                break;
        }

        ReportStatusToSCMgr(_ss.dwCurrentState, 0, 0);
    }

    /**
     * This method should be called only by the SCM.
     */
    extern (Windows) static export void service_main(uint dwArgc, char **args)
    {
        _args.length = dwArgc;
        for (uint u = 0; u < dwArgc; u++)
            _args[u] = std.string.toString(args[u]);
        _ssh = RegisterServiceCtrlHandlerA(toStringz(_serviceName), &service_ctrl);

        // SERVICE_STATUS members that don't change in example
        _ss.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
        _ss.dwServiceSpecificExitCode = 0;


        // report the status to the service control manager.
        if (!ReportStatusToSCMgr(
                           SERVICE_STATES.SERVICE_START_PENDING,    // service state
                           0,                                       // exit code
                           3000))                                   // wait hint
        {
            if (_ssh)
                ReportStatusToSCMgr(SERVICE_STATES.SERVICE_STOPPED, 0, 0);
        }

        _self.ServiceStart(_args);
    }

    /**
     * This method can be called by a derived class during service stop to keep
     * the SCM appraised of progress.
     */
    public static bool ReportStatusToSCMgr(uint dwCurrentState,
                                    uint dwWin32ExitCode,
                                    uint dwWaitHint)
    {
        uint dwCheckPoint = 1;
        bool fResult = true;


        if (!_debug) // when debugging we don't report to the SCM
        {
            if (dwCurrentState == SERVICE_STATES.SERVICE_START_PENDING)
                _ss.dwControlsAccepted = 0;
            else
                _ss.dwControlsAccepted = ACCEPT_COMMANDS.SERVICE_ACCEPT_STOP;

            _ss.dwCurrentState = dwCurrentState;
            _ss.dwWin32ExitCode = dwWin32ExitCode;
            _ss.dwWaitHint = dwWaitHint;

            if (dwCurrentState == SERVICE_STATES.SERVICE_RUNNING ||
                dwCurrentState == SERVICE_STATES.SERVICE_STOPPED)
                _ss.dwCheckPoint = 0;
             else
                _ss.dwCheckPoint = dwCheckPoint++;


            // Report the status of the service to the service control manager.
            //
            fResult = (SetServiceStatus( _ssh, &_ss) == 1);
        }
        return fResult;
    }


    /**
     * This method should be called by your main() function to implement the service.
     */
    static void implementMain(char[][] args)
    {
        if (args.length == 2 && (args[1][0] == '-' || args[1][0] == '/'))
        {
            if (args[1][1] == 'i' || args[1][1] == 'I')
            {
                writefln("Installing service: %s", ServiceBase.Name);
                ServiceBase.InstallService();
                return;
            }
            else if (args[1][1] == 'r' || args[1][1] == 'R')
            {
                writefln("Removing service: %s", ServiceBase.Name);
                ServiceBase.RemoveService();
                return;
            }
            else if (args[1][1] == 'd' || args[1][1] == 'D')
            {
                writefln("Service - debug mode requested");
                return;
            }
            else
            {
                writefln("Usage: <service Name> [-i|-I|-r|-R|-d|-D|/i|/I|/r|/R|/d|/D]");
                return;
            }
        }
        else if (args.length == 1)
            ServiceBase.StartService();
        else
            writefln("Usage: <service Name> [-i|-I|-r|-R|-d|-D|/i|/I|/r|/R|/d|/D]");
    }
}

/+
// Derive a class from ServiceBase implementing the abstract methods.
class NullService : ServiceBase
{
    this()
    {
        super("Dummy", false);
    }

    public void onServiceStart(char[][] args)
    {
        ServiceBase.EventLog.logMessage("NullService onServiceStart was called");
    }

    public void onServiceStop()
    {
        ServiceBase.EventLog.logMessage("NullService onServiceStop was called");
    }
}

// Create an object of the derived class, then call the base class implementMain method.
void main(char[][] args)
{
    try
    {
        ServiceBase sb = new NullService();
        ServiceBase.implementMain(args);
    }
    catch (Exception ex)
    {
        writefln(ex.toString());
    }
}
+/
