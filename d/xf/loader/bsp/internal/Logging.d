module xf.loader.bsp.internal.Logging;
import xf.utils.log.Log;

mixin LibraryLog!("bspLogger", "BSPDebug", "Debug4","Debug3","Debug2","Debug1", LogLevel.Trace, LogLevel.Info, LogLevel.Warn, LogLevel.Error, LogLevel.Fatal);