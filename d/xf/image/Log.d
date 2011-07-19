module xf.image.Log;
import xf.utils.log.Log;

mixin LibraryLog!("imageLogger", "ImageDebug", "Debug4","Debug3","Debug2","Debug1", LogLevel.Trace, LogLevel.Info, LogLevel.Warn, LogLevel.Error, LogLevel.Fatal);