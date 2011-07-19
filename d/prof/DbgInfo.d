module xf.prof.DbgInfo;import xf.utils.Memory;
import tango.text.Util;
import tango.stdc.stdio;
import tango.stdc.stringz;
import tango.stdc.string : strcpy;
import tango.sys.win32.CodePage;
import tango.core.Exception;



struct AddrDebugInfo {
	align(1) {
		size_t	addr;
		char*	file;
		char*	func;
		ushort	line;
	}
}

class ModuleDebugInfo {
	AddrDebugInfo[]	debugInfo;
	uint						debugInfoLen;
	size_t[char*]			fileMaxAddr;
	char*[]					strBuffer;
	uint						strBufferLen;
	
	void addDebugInfo(size_t addr, char* file, char* func, ushort line) {
		debugInfo.append(AddrDebugInfo(addr, file, func, line), &debugInfoLen);

		if (auto a = file in fileMaxAddr) {
			if (addr > *a) *a = addr;
		} else {
			fileMaxAddr[file] = addr;
		}
	}
	
	char* bufferString(char[] str) {
		char[] res;
		res.alloc(str.length+1, false);
		res[0..$-1] = str[];
		res[str.length] = 0;
		strBuffer.append(res.ptr, &strBufferLen);
		return res.ptr;
	}
	
	void freeArrays() {
		debugInfo.free();
		debugInfoLen = 0;

		fileMaxAddr = null;
		foreach (ref s; strBuffer[0..strBufferLen]) {
			cFree(s);
		}
		strBuffer.free();
		strBufferLen = 0;
	}
	
	ModuleDebugInfo	prev;
	ModuleDebugInfo	next;
}

class GlobalDebugInfo {
	ModuleDebugInfo	head;
	ModuleDebugInfo	tail;
	
	
	synchronized int opApply(int delegate(ref ModuleDebugInfo) dg) {
		for (auto it = head; it !is null; it = it.next) {
			if (auto res = dg(it)) {
				return res;
			}
		}
		return 0;
	}
	
	
	synchronized void addDebugInfo(ModuleDebugInfo info) {
		if (head is null) {
			head = tail = info;
			info.next = info.prev = null;
		} else {
			tail.next = info;
			info.prev = tail;
			info.next = null;
			tail = info;
		}
	}
	
	
	synchronized void removeDebugInfo(ModuleDebugInfo info) {
		assert (info !is null);
		assert (info.next !is null || info.prev !is null || head is info);
		
		if (info is head) {
			head = head.next;
		}
		if (info is tail) {
			tail = tail.prev;
		}
		if (info.prev) {
			info.prev.next = info.next;
		}
		if (info.next) {
			info.next.prev = info.prev;
		}
		info.freeArrays;
		info.prev = info.next = null;
		
		delete info;
	}
}

private GlobalDebugInfo globalDebugInfo;
static this() {
	globalDebugInfo = new GlobalDebugInfo;
}

void initDebugInfo(char[] progName) {
	scope info = new DebugInfo(progName);
	// we'll let it die now :)
}


AddrDebugInfo getAddrDbgInfo(size_t a, ptrdiff_t* diff = null) {
    AddrDebugInfo bestInfo;
    int minDiff = 0x7fffffff;
    int bestOff = 0;
	const int addBias = 0;
    
    foreach (modInfo; globalDebugInfo) {
		bool local = false;
		
		foreach (l; modInfo.debugInfo[0 .. modInfo.debugInfoLen]) {
			int diff = a - l.addr - addBias;
			
			// increasing it will make the lookup give results 'higher' in the code (at lower addresses)
			// using the value of 1 is recommended when not using version StacktraceTryMatchCallAddresses,
			// but it may result in AVs reporting an earlier line in the source code
			const int minSymbolOffset = 0;
			
			if (diff < minSymbolOffset) {
				continue;
			}
			
			int absdiff = diff > 0 ? diff : -diff;
			if (absdiff < minDiff) {
				minDiff = absdiff;
				bestOff = diff;
				bestInfo = l;
				local = true;
			}
		}
		
		if (local) {
			if (minDiff > 0x100) {
				bestInfo = bestInfo.init;
				minDiff = 0x7fffffff;
			}
			else {
				if (auto ma = bestInfo.file in modInfo.fileMaxAddr) {
					if (a > *ma+addBias) {
						bestInfo = bestInfo.init;
						minDiff = 0x7fffffff;
					}
				} else {
					printf("there ain't '%s' in fileMaxAddr\n", bestInfo.file);
					bestInfo = bestInfo.init;
					minDiff = 0x7fffffff;
				}
			}
		}
	}
	
	if (diff !is null) {
		*diff = bestOff;
	}
    return bestInfo;
}

   

class DebugInfo {
	ModuleDebugInfo info;
	
	
	this(char[] filename) {
		info = new ModuleDebugInfo;
		ParseCVFile(filename);
		assert (globalDebugInfo !is null);
		globalDebugInfo.addDebugInfo(info);
	}
	 
	private {
		int ParseCVFile(char[] filename) {
			FILE* debugfile;

			if (filename == "") return (-1);

			//try {
				debugfile = fopen((filename ~ \0).ptr, "rb");
			/+} catch(Exception e){
				return -1;
			}+/

			if (!ParseFileHeaders (debugfile)) return -1;

			g_secthdrs.length = g_nthdr.FileHeader.NumberOfSections;

			if (!ParseSectionHeaders (debugfile)) return -1;

			g_debugdirs.length = g_nthdr.OptionalHeader.DataDirectory[IMAGE_FILE_DEBUG_DIRECTORY].Size /
				IMAGE_DEBUG_DIRECTORY.sizeof;

			if (!ParseDebugDir (debugfile)) return -1;
			if (g_dwStartOfCodeView == 0) return -1;
			if (!ParseCodeViewHeaders (debugfile)) return -1;
			if (!ParseAllModules (debugfile)) return -1;

			g_dwStartOfCodeView = 0;
			g_exe_mode = true;
			g_secthdrs = null;
			g_debugdirs = null;
			g_cvEntries = null;
			g_cvModules = null;
			g_filename = null;
			g_filenameStringz = null;

			fclose(debugfile);
			return 0;
		}
			
		bool ParseFileHeaders(FILE* debugfile) {
			CVHeaderType hdrtype;

			hdrtype = GetHeaderType (debugfile);

			if (hdrtype == CVHeaderType.DOS) {
				if (!ReadDOSFileHeader (debugfile, &g_doshdr))return false;
				hdrtype = GetHeaderType (debugfile);
			}
			if (hdrtype == CVHeaderType.NT) {
				if (!ReadPEFileHeader (debugfile, &g_nthdr)) return false;
			}

			return true;
		}
			
		CVHeaderType GetHeaderType(FILE* debugfile) {
			ushort hdrtype;
			CVHeaderType ret = CVHeaderType.NONE;

			int oldpos = ftell(debugfile);

			if (!ReadChunk (debugfile, &hdrtype, ushort.sizeof, -1)){
				fseek(debugfile, oldpos, SEEK_SET);
				return CVHeaderType.NONE;
			}

			if (hdrtype == 0x5A4D) 	     // "MZ"
				ret = CVHeaderType.DOS;
			else if (hdrtype == 0x4550)  // "PE"
				ret = CVHeaderType.NT;
			else if (hdrtype == 0x4944)  // "DI"
				ret = CVHeaderType.DBG;

			fseek(debugfile, oldpos, SEEK_SET);

			return ret;
		}
		 
		/*
		 * Extract the DOS file headers from an executable
		 */
		bool ReadDOSFileHeader(FILE* debugfile, IMAGE_DOS_HEADER *doshdr) {
			uint bytes_read;

			bytes_read = fread(doshdr, 1, IMAGE_DOS_HEADER.sizeof, debugfile);
			if (bytes_read < IMAGE_DOS_HEADER.sizeof){
				return false;
			}

			// Skip over stub data, if present
			if (doshdr.e_lfanew) {
				fseek(debugfile, doshdr.e_lfanew, SEEK_SET);
			}

			return true;
		}
		 
		/*
		 * Extract the DOS and NT file headers from an executable
		 */
		bool ReadPEFileHeader(FILE* debugfile, IMAGE_NT_HEADERS *nthdr) {
			uint bytes_read;

			bytes_read = fread(nthdr, 1, IMAGE_NT_HEADERS.sizeof, debugfile);
			if (bytes_read < IMAGE_NT_HEADERS.sizeof) {
				return false;
			}

			return true;
		}
		  
		bool ParseSectionHeaders(FILE* debugfile) {
			if (!ReadSectionHeaders (debugfile, g_secthdrs)) return false;
			return true;
		}
			
		bool ReadSectionHeaders(FILE* debugfile, inout IMAGE_SECTION_HEADER[] secthdrs) {
			for(int i=0;i<secthdrs.length;i++){
				uint bytes_read;
				bytes_read = fread((&secthdrs[i]), 1, IMAGE_SECTION_HEADER.sizeof, debugfile);
				if (bytes_read < 1){
					return false;
				}
			}
			return true;
		}
		  
		bool ParseDebugDir(FILE* debugfile) {
			int i;
			int filepos;

			if (g_debugdirs.length == 0) return false;

			filepos = GetOffsetFromRVA (g_nthdr.OptionalHeader.DataDirectory[IMAGE_FILE_DEBUG_DIRECTORY].VirtualAddress);

			fseek(debugfile, filepos, SEEK_SET);

			if (!ReadDebugDir (debugfile, g_debugdirs)) return false;

			for (i = 0; i < g_debugdirs.length; i++) {
				enum {
					IMAGE_DEBUG_TYPE_CODEVIEW = 2,
				}

				if (g_debugdirs[i].Type == IMAGE_DEBUG_TYPE_CODEVIEW) {
					g_dwStartOfCodeView = g_debugdirs[i].PointerToRawData;
				}
			}

			g_debugdirs = null;

			return true;
		}
			
		// Calculate the file offset, based on the RVA.
		uint GetOffsetFromRVA(uint rva) {
			int i;
			uint sectbegin;

			for (i = g_secthdrs.length - 1; i >= 0; i--) {
				sectbegin = g_secthdrs[i].VirtualAddress;
				if (rva >= sectbegin) break;
			}
			uint offset = g_secthdrs[i].VirtualAddress - g_secthdrs[i].PointerToRawData;
			uint filepos = rva - offset;
			return filepos;
		}
		 
		// Load in the debug directory table.  This directory describes the various
		// blocks of debug data that reside at the end of the file (after the COFF
		// sections), including FPO data, COFF-style debug info, and the CodeView
		// we are *really* after.
		bool ReadDebugDir(FILE* debugfile, inout IMAGE_DEBUG_DIRECTORY debugdirs[]) {
			uint bytes_read;
			for(int i=0;i<debugdirs.length;i++) {
				bytes_read = fread((&debugdirs[i]), 1, IMAGE_DEBUG_DIRECTORY.sizeof, debugfile);
				if (bytes_read < IMAGE_DEBUG_DIRECTORY.sizeof) {
					return false;
				}
			}
			return true;
		}
		  
		bool ParseCodeViewHeaders(FILE* debugfile) {
			fseek(debugfile, g_dwStartOfCodeView, SEEK_SET);
			if (!ReadCodeViewHeader (debugfile, g_cvSig, g_cvHeader)) return false;
			g_cvEntries.length = g_cvHeader.cDir;
			if (!ReadCodeViewDirectory (debugfile, g_cvEntries)) return false;
			return true;
		}

			
		bool ReadCodeViewHeader(FILE* debugfile, out OMFSignature sig, out OMFDirHeader dirhdr) {
			uint bytes_read;

			bytes_read = fread((&sig), 1, OMFSignature.sizeof, debugfile);
			if (bytes_read < OMFSignature.sizeof){
				return false;
			}

			fseek(debugfile, sig.filepos + g_dwStartOfCodeView, SEEK_SET);
			bytes_read = fread((&dirhdr), 1, OMFDirHeader.sizeof, debugfile);
			if (bytes_read < OMFDirHeader.sizeof){
				return false;
			}
			return true;
		}
		 
		bool ReadCodeViewDirectory(FILE* debugfile, inout OMFDirEntry[] entries) {
			uint bytes_read;

			for(int i=0;i<entries.length;i++){
				bytes_read = fread((&entries[i]), 1, OMFDirEntry.sizeof, debugfile);
				if (bytes_read < OMFDirEntry.sizeof){
					return false;
				}
			}
			return true;
		}
		  
		bool ParseAllModules (FILE* debugfile) {
			if (g_cvHeader.cDir == 0){
				return true;
			}

			if (g_cvEntries.length == 0){
				return false;
			}

			fseek(debugfile, g_dwStartOfCodeView + g_cvEntries[0].lfo, SEEK_SET);

			if (!ReadModuleData (debugfile, g_cvEntries, g_cvModules)){
				return false;
			}


			for (int i = 0; i < g_cvModules.length; i++){
				ParseRelatedSections (i, debugfile);
			}

			return true;
		}

			
		bool ReadModuleData(FILE* debugfile, OMFDirEntry[] entries, out OMFModuleFull[] modules) {
			uint bytes_read;
			int pad;

			int module_bytes = (ushort.sizeof * 3) + (char.sizeof * 2);

			if (entries == null) return false;

			modules.length = 0;

			for (int i = 0; i < entries.length; i++){
				if (entries[i].SubSection == sstModule)
					modules.length = modules.length + 1;
			}

			for (int i = 0; i < modules.length; i++){

				bytes_read = fread((&modules[i]), 1, module_bytes, debugfile);
				if (bytes_read < module_bytes){
					return false;
				}

				int segnum = modules[i].cSeg;
				OMFSegDesc[] segarray;
				segarray.length=segnum;
				for(int j=0;j<segnum;j++){
					bytes_read =  fread((&segarray[j]), 1, OMFSegDesc.sizeof, debugfile);
					if (bytes_read < OMFSegDesc.sizeof){
						return false;
					}
				}
				modules[i].SegInfo = segarray.ptr;

				char namelen;
				bytes_read = fread((&namelen), 1, char.sizeof, debugfile);
				if (bytes_read < 1){
					return false;
				}

				pad = ((namelen + 1) % 4);
				if (pad) namelen += (4 - pad);

				modules[i].Name = (new char[namelen+1]).ptr;
				modules[i].Name[namelen]=0;
				bytes_read = fread((modules[i].Name), 1, namelen, debugfile);
				if (bytes_read < namelen){
					return false;
				}
			}
			return true;
		}
		 
		bool ParseRelatedSections(int index, FILE* debugfile) {
			int i;

			if (g_cvEntries == null)
				return false;

			for (i = 0; i < g_cvHeader.cDir; i++){
				if (g_cvEntries[i].iMod != (index + 1) ||
					g_cvEntries[i].SubSection == sstModule)
					continue;

				switch (g_cvEntries[i].SubSection){
				case sstSrcModule:
					ParseSrcModuleInfo (i, debugfile);
					break;
				default:
					break;
				}
			}

			return true;
		}
			
		bool ParseSrcModuleInfo (int index, FILE* debugfile) {
			int i;

			byte *rawdata;
			byte *curpos;
			short filecount;
			short segcount;

			int moduledatalen;
			int filedatalen;
			int linedatalen;

			if (g_cvEntries == null || debugfile == null ||
				g_cvEntries[index].SubSection != sstSrcModule)
				return false;

			int fileoffset = g_dwStartOfCodeView + g_cvEntries[index].lfo;

			rawdata = (new byte[g_cvEntries[index].cb]).ptr;
			if (!rawdata) return false;

			if (!ReadChunk (debugfile, rawdata, g_cvEntries[index].cb, fileoffset)) return false;
			uint[] baseSrcFile;
			ExtractSrcModuleInfo (rawdata, &filecount, &segcount,baseSrcFile);

			for(i=0;i<baseSrcFile.length;i++){
				uint baseSrcLn[];
				ExtractSrcModuleFileInfo (rawdata+baseSrcFile[i],baseSrcLn);
				for(int j=0;j<baseSrcLn.length;j++){
					ExtractSrcModuleLineInfo (rawdata+baseSrcLn[j], j);
				}
			}

			return true;
		}
		
		void ExtractSrcModuleInfo (byte* rawdata, short *filecount, short *segcount,out uint[] fileinfopos) {
			int i;
			int datalen;

			ushort cFile;
			ushort cSeg;
			uint *baseSrcFile;
			uint *segarray;
			ushort *segindexarray;

			cFile = *cast(short*)rawdata;
			cSeg = *cast(short*)(rawdata + 2);
			baseSrcFile = cast(uint*)(rawdata + 4);
			segarray = &baseSrcFile[cFile];
			segindexarray = cast(ushort*)(&segarray[cSeg * 2]);

			*filecount = cFile;
			*segcount = cSeg;

			fileinfopos.length=cFile;
			for (i = 0; i < cFile; i++) {
				fileinfopos[i]=baseSrcFile[i];
			}
		}
		 
		void ExtractSrcModuleFileInfo(byte* rawdata,out uint[] offset) {
			int i;
			int datalen;

			ushort cSeg;
			uint *baseSrcLn;
			uint *segarray;
			byte cFName;

			cSeg = *cast(short*)(rawdata);
			// Skip the 'pad' field
			baseSrcLn = cast(uint*)(rawdata + 4);
			segarray = &baseSrcLn[cSeg];
			cFName = *(cast(byte*)&segarray[cSeg*2]);

			g_filename = (cast(char*)&segarray[cSeg*2] + 1)[0..cFName].dup;
			g_filenameStringz = info.bufferString(g_filename);

			offset.length=cSeg;
			for (i = 0; i < cSeg; i++){
				offset[i]=baseSrcLn[i];
			}
		}
		 
		void ExtractSrcModuleLineInfo(byte* rawdata, int tablecount) {
			int i;

			ushort Seg;
			ushort cPair;
			uint *offset;
			ushort *linenumber;

			Seg = *cast(ushort*)rawdata;
			cPair = *cast(ushort*)(rawdata + 2);
			offset = cast(uint*)(rawdata + 4);
			linenumber = cast(ushort*)&offset[cPair];

			uint base=0;
			if (Seg != 0){
				base = g_nthdr.OptionalHeader.ImageBase+g_secthdrs[Seg-1].VirtualAddress;
			}
			
			for (i = 0; i < cPair; i++) {
				uint address = offset[i]+base;
				info.addDebugInfo(address, g_filenameStringz, null, linenumber[i]);
			}
		}

		   
		bool ReadChunk(FILE* debugfile, void *dest, int length, int fileoffset) {
			uint bytes_read;

			if (fileoffset >= 0) {
				fseek(debugfile, fileoffset, SEEK_SET);
			}

			bytes_read = fread(dest, 1, length, debugfile);
			if (bytes_read < length) {
				return false;
			}

			return true;
		}


		enum CVHeaderType : int {
			NONE,
			DOS,
			NT,
			DBG
		}

		int g_dwStartOfCodeView = 0;

		bool g_exe_mode = true;
		IMAGE_DOS_HEADER g_doshdr;
		IMAGE_SEPARATE_DEBUG_HEADER g_dbghdr;
		IMAGE_NT_HEADERS g_nthdr;

		IMAGE_SECTION_HEADER g_secthdrs[];

		IMAGE_DEBUG_DIRECTORY g_debugdirs[];
		OMFSignature g_cvSig;
		OMFDirHeader g_cvHeader;
		OMFDirEntry g_cvEntries[];
		OMFModuleFull g_cvModules[];
		char[] g_filename;
		char* g_filenameStringz;
	}
}




enum {
	IMAGE_FILE_DEBUG_DIRECTORY = 6
}
 
enum {
	sstModule			= 0x120,
	sstSrcModule		= 0x127,
	sstGlobalPub		= 0x12a,
}
 
struct OMFSignature {
	char	Signature[4];
	int	filepos;
}
 
struct OMFDirHeader {
	ushort	cbDirHeader;
	ushort	cbDirEntry;
	uint	cDir;
	int		lfoNextDir;
	uint	flags;
}
 
struct OMFDirEntry {
	ushort	SubSection;
	ushort	iMod;
	int		lfo;
	uint	cb;
}
  
struct OMFSegDesc {
	ushort	Seg;
	ushort	pad;
	uint	Off;
	uint	cbSeg;
}
 
struct OMFModule {
	ushort	ovlNumber;
	ushort	iLib;
	ushort	cSeg;
	char			Style[2];
}
 
struct OMFModuleFull {
	ushort	ovlNumber;
	ushort	iLib;
	ushort	cSeg;
	char			Style[2];
	OMFSegDesc		*SegInfo;
	char			*Name;
}
	
struct OMFSymHash {
	ushort	symhash;
	ushort	addrhash;
	uint	cbSymbol;
	uint	cbHSym;
	uint	cbHAddr;
}
 
struct DATASYM16 {
		ushort reclen;	// Record length
		ushort rectyp;	// S_LDATA or S_GDATA
		int off;		// offset of symbol
		ushort seg;		// segment of symbol
		ushort typind;	// Type index
		byte name[1];	// Length-prefixed name
}
typedef DATASYM16 PUBSYM16;
 

struct IMAGE_DOS_HEADER {      // DOS .EXE header
    ushort   e_magic;                     // Magic number
    ushort   e_cblp;                      // Bytes on last page of file
    ushort   e_cp;                        // Pages in file
    ushort   e_crlc;                      // Relocations
    ushort   e_cparhdr;                   // Size of header in paragraphs
    ushort   e_minalloc;                  // Minimum extra paragraphs needed
    ushort   e_maxalloc;                  // Maximum extra paragraphs needed
    ushort   e_ss;                        // Initial (relative) SS value
    ushort   e_sp;                        // Initial SP value
    ushort   e_csum;                      // Checksum
    ushort   e_ip;                        // Initial IP value
    ushort   e_cs;                        // Initial (relative) CS value
    ushort   e_lfarlc;                    // File address of relocation table
    ushort   e_ovno;                      // Overlay number
    ushort   e_res[4];                    // Reserved words
    ushort   e_oemid;                     // OEM identifier (for e_oeminfo)
    ushort   e_oeminfo;                   // OEM information; e_oemid specific
    ushort   e_res2[10];                  // Reserved words
    int      e_lfanew;                    // File address of new exe header
}
 
struct IMAGE_FILE_HEADER {
    ushort    Machine;
    ushort    NumberOfSections;
    uint      TimeDateStamp;
    uint      PointerToSymbolTable;
    uint      NumberOfSymbols;
    ushort    SizeOfOptionalHeader;
    ushort    Characteristics;
}
 
struct IMAGE_SEPARATE_DEBUG_HEADER {
    ushort        Signature;
    ushort        Flags;
    ushort        Machine;
    ushort        Characteristics;
    uint       TimeDateStamp;
    uint       CheckSum;
    uint       ImageBase;
    uint       SizeOfImage;
    uint       NumberOfSections;
    uint       ExportedNamesSize;
    uint       DebugDirectorySize;
    uint       SectionAlignment;
    uint       Reserved[2];
}
 
struct IMAGE_DATA_DIRECTORY {
    uint   VirtualAddress;
    uint   Size;
}
 
struct IMAGE_OPTIONAL_HEADER {
    //
    // Standard fields.
    //

    ushort    Magic;
    byte    MajorLinkerVersion;
    byte    MinorLinkerVersion;
    uint   SizeOfCode;
    uint   SizeOfInitializedData;
    uint   SizeOfUninitializedData;
    uint   AddressOfEntryPoint;
    uint   BaseOfCode;
    uint   BaseOfData;

    //
    // NT additional fields.
    //

    uint   ImageBase;
    uint   SectionAlignment;
    uint   FileAlignment;
    ushort    MajorOperatingSystemVersion;
    ushort    MinorOperatingSystemVersion;
    ushort    MajorImageVersion;
    ushort    MinorImageVersion;
    ushort    MajorSubsystemVersion;
    ushort    MinorSubsystemVersion;
    uint   Win32VersionValue;
    uint   SizeOfImage;
    uint   SizeOfHeaders;
    uint   CheckSum;
    ushort    Subsystem;
    ushort    DllCharacteristics;
    uint   SizeOfStackReserve;
    uint   SizeOfStackCommit;
    uint   SizeOfHeapReserve;
    uint   SizeOfHeapCommit;
    uint   LoaderFlags;
    uint   NumberOfRvaAndSizes;

	enum {
		IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16,
	}

    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
}
 
struct IMAGE_NT_HEADERS {
    uint Signature;
    IMAGE_FILE_HEADER FileHeader;
    IMAGE_OPTIONAL_HEADER OptionalHeader;
}
 
enum {
	IMAGE_SIZEOF_SHORT_NAME = 8,
}

struct IMAGE_SECTION_HEADER {
    byte    Name[IMAGE_SIZEOF_SHORT_NAME];//8
    union misc{
            uint   PhysicalAddress;
            uint   VirtualSize;//12
    }
	misc Misc;
    uint   VirtualAddress;//16
    uint   SizeOfRawData;//20
    uint   PointerToRawData;//24
    uint   PointerToRelocations;//28
    uint   PointerToLinenumbers;//32
    ushort NumberOfRelocations;//34
    ushort NumberOfLinenumbers;//36
    uint   Characteristics;//40
}
 
struct IMAGE_DEBUG_DIRECTORY {
    uint   Characteristics;
    uint   TimeDateStamp;
    ushort MajorVersion;
    ushort MinorVersion;
    uint   Type;
    uint   SizeOfData;
    uint   AddressOfRawData;
    uint   PointerToRawData;
}
 
struct OMFSourceLine {
	ushort	Seg;
	ushort	cLnOff;
	uint	offset[1];
	ushort	lineNbr[1];
}
 
struct OMFSourceFile {
	ushort	cSeg;
	ushort	reserved;
	uint	baseSrcLn[1];
	ushort	cFName;
	char	Name;
}
 
struct OMFSourceModule {
	ushort	cFile;
	ushort	cSeg;
	uint	baseSrcFile[1];
}
