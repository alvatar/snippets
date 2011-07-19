module xf.input.Writer;

version(Windows) {
	public import xf.input.writer.Win32 : OSInputWriter = Win32InputWriter;
} else {
	public import xf.input.writer.X : OSInputWriter = XInputWriter;
}
