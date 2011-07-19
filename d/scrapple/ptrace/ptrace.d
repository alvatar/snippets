module ptrace;

import tango.io.FileConduit;
import tango.io.File;
import tango.io.Stdout;
import tango.text.convert.Integer;
import tango.core.Array;
import tango.stdc.ctype;
import tango.util.Arguments;
import tango.text.stream.LineIterator;
import tango.sys.Process;
import tango.sys.Environment;
import tango.stdc.stdio;
import Regex        = tango.text.Regex;
import Util         = tango.text.Util;
import ConvertFloat = tango.text.convert.Float;



enum SymbolMode { FQN, SEP, SYM }

SymbolMode timingsMode = SymbolMode.SEP;
SymbolMode graphMode = SymbolMode.SEP;

char[]  sourceFile;
char[]  targetFile;
char[]  targetTimingsFile;
char[]  targetGraphFile;

bool    showHelp = false;
bool    onePass = false;

enum ParseMode
{
    Full,
    QualifiedName
}

ParseMode mode = ParseMode.QualifiedName;
char[]  helpTxt =
                 "  usage: ptrace [file] [options]\n"
                 "    -t=<target>      write to target, must be with d extension for dmd\n"
                 "    -s=<mode>        specifies how to write symbols, <mode> can be of one:\n"
                 "                       FQN    fully qualified name\n"
                 "                       SYM    identifier only, not type information\n"
                 "                       SEP    seperate columns for type and identifier\n"
                 "    -h               this help text\n"
                 "    -t=<target>      write to target, must be with d extension for dmd\n"
                 "    -f               do not ask for permission to overwrite existing files\n"
                 "    -x               eXecute dmd and generate html instead of ddoc code\n\n"
                 "  if no source file has been given, trace.log is assumed\n"
                 "  if no target file has been given, -x is assumed\n";


/* TODO: implement these options correctly
"    -st=<mode>       idem, but only for timings\n"
"    -sg=<mode>       idem, but only for graph\n"
"    -tt=<target>     write only timings to target, must be with d extension for dmd\n"
                 "    -tg=<target>     write only call graph to target, must be with d extension for dmd\n"
*/

bool parseArgs(char[][] args)
{
    bool force = false;

    auto arguments = new Arguments(args, ["source"]);
    if (arguments["source"].length == 1)
        sourceFile = arguments["source"][0];
    else if (arguments["source"].length > 1)
    {
        Stdout("wrong number of source files").newline;
        showHelp = true;
    }
    else
        sourceFile = "trace.log";

    with(arguments)
    {
        addValidation("h", false, false);
        addValidation("s", false, true);
        addValidation("t", false, true);
        addValidation("f", false, false);
        addValidation("x", false, false);
    }

    if ("h" in arguments)
        showHelp = true;

    if ("x" in arguments)
        onePass = true;

    if ("f" in  arguments)
        force = true;

    if ("s" in arguments)
    {
        switch(arguments["s"][0])
        {
            case "FQN": graphMode = timingsMode = SymbolMode.FQN; break;
            case "SYM": graphMode = timingsMode = SymbolMode.SYM; break;
            case "SEP": graphMode = timingsMode = SymbolMode.SEP; break;
            default: showHelp = true; break;
        }
    }
    if (arguments["t"].length)
        targetFile = arguments["t"][0];
    else
    {
        targetFile = "ddoc_" ~ sourceFile ~ ".d";
        onePass = true;
    }

    if( (new FilePath(targetFile)).exists() && !force)
    {
        char response;
        while (true)
        {
            Stdout.format("Are you sure you want to overwrite {} (y/n)?", targetFile).newline;
            response = getchar();
            if (response == 'n' || response == 'N')
                return false;
            else if (response == 'y' || response == 'Y')
                break;
        }
    }

    return true;
}


int main(char[][] args)
{
    Stdout("Ptrace. Utility to prettify trace.log files. ptrace -h for help.\n");
    if (!parseArgs(args))
        return 0;
    if (showHelp)
    {
        Stdout(helpTxt);
        return 0;
    }

    //Stdout.format("Reading from {0} and writing to {1}", sourceFile, targetFile).newline;

    auto reader = new TraceReader(sourceFile, new TraceParser);

    reader.read;

    auto output = new FileConduit(targetFile, FileConduit.ReadWriteCreate);

    char[] header()
    {
        switch (timingsMode)
        {
            case SymbolMode.FQN: return " $(HEADER_FUNCTION_NAME symbol)\n";
            case SymbolMode.SYM: return " $(HEADER_FUNCTION_NAME identifier)\n";
            case SymbolMode.SEP: return " $(HEADER_FUNCTION_NAME identifier) $(HEADER_FUNCTION_NAME type) \n";
            default: assert(false); break;
        }
    }

    Stdout("writing output to " ~ targetFile ~ "...").newline;
    output.output.write("Ddoc\n");

    if (onePass)
    {
        output.output.write(macros);
    }

    output.output.write("\n$(TIMINGS $(HEADER_CALLS Num calls) $(HEADER_TREE  Tree Time)"
                        "$(HEADER_FUNC  Func Time) $(HEADER_CALL Per Call)" ~ header() );

    char[] functionName(char[] mangled)
    {
        switch(timingsMode)
        {
            case SymbolMode.SYM: return " $(FUNCTION_NAME " ~ reader.getSYM(mangled) ~ ") )\n";
            case SymbolMode.FQN: return " $(FUNCTION_NAME " ~ reader.getFQN(mangled) ~ ") )\n";
            case SymbolMode.SEP: return " $(FUNCTION_NAME " ~ reader.getSYM(mangled) ~  ") "
                                        " $(FUNCTION_NAME " ~ reader.getTYPE(mangled) ~  ") )\n";
            default: assert(false); break;
        }
    }

    foreach(timing; reader.timings)
        output.output.write( " $(TIMING  $(CALLS " ~ toString(timing.calls) ~ ")"
                             " $(TREE  " ~ toString(timing.tree) ~ ") $(FUNC  " ~ toString(timing.func) ~ ")"
                             " $(CALL  " ~ toString(timing.call) ~ ")" ~ functionName(timing.mangled) );

    output.output.write(")\n $(CALL_GRAPH \n");

/*
TODO:
make call graph output fqn/sym/type of symbol correctly: specifify colspan in trace.ddoc macro's

*/

    foreach(node; reader.parser.nodes)
    {
        output.output.write("$(CALL_SEP )\n");
        foreach(fun; node.fan_in)
            output.output.write("$(FAN $(FAN_FUNC " ~ reader.getFQN(fun.mangled) ~ ") $(FAN_CALLS " ~ toString(fun.numCalls) ~ "))\n");

        output.output.write("$(CALL_FUN $(CALL_FUN_NAME 2, " ~ reader.getFQN(node.mangled) ~
                            ") $(CALL_FUN_TREE  " ~ ConvertFloat.toString(cast(real)node.tree / reader.freq, 2, false ) ~ ")"
                            " $(CALL_FUN_FUNC " ~ ConvertFloat.toString(cast(real)node.func / reader.freq, 2, false) ~ ")"
                            " $(CALL_FUN_CALLS " ~ toString(node.count) ~ ") )\n" );

        foreach(fun; node.fan_out)
            output.output.write("$(FAN $(FAN_FUNC " ~ reader.demangleSymbol(fun.mangled) ~ ") $(FAN_CALLS  " ~ toString(fun.numCalls) ~ ") )\n");

    }
    output.output.write(")\n");
    output.close;

    if (onePass)
    {
        Stdout("generating html...").newline;
        Process dmd = new Process(Environment.exePath("dmd").toString, "-D"[], targetFile, null);
        dmd.execute();
        auto result = dmd.wait();
        Stdout("removing temporary files...").newline;
        (new FilePath(targetFile)).remove();
        return result.status;
    }
    return 0;
}

struct FuncCall
{
    char[] mangled;
    uint numCalls;
}

struct CallNode
{
    char[] mangled;
    FuncCall[] fan_in;
    FuncCall[] fan_out;
    ulong count;
    ulong tree;
    ulong func;
}

struct Timing
{
    char[] mangled;
    ulong calls;
    // timings:
    ulong tree;
    ulong func;
    ulong call;
}

class TraceParser
{
    void processCall(char[][] fan_in, char[] func, char[][] fan_out)
    {
        auto isSpace = &Util.isSpace!(char);
        nodes.length = nodes.length + 1;

        void parseFan(ref FuncCall[] calls, ref char[][] data)
        {
            foreach (line; data)
            {
                int numcalls;

                int index = findIf(line, &Util.isSpace!(char));

                if (index > 0)
                    numcalls = toLong(line[0..index]);

                index = rfindIf(line, isSpace);
                if (index > 0)
                {
                   calls ~= FuncCall(Util.trim(line[index..$]), numcalls);
                }
            }
        }

        parseFan(nodes[$-1].fan_in, fan_in);
        parseFan(nodes[$-1].fan_out, fan_out);

        // function symbol
        int index = findIf(func, isSpace);
        if (index > 0)
            nodes[$-1].mangled = Util.trim(func[0..index]).dup;

        // tree time
        func = Util.trim(func[index..$]);
        index = findIf(func, isSpace);

        if (index > 0)
            nodes[$-1].count = toLong(func[0..index]);

        // function time
        func = Util.trim(func[index..$]);
        index = rfindIf(func, isSpace);
        if (index > 0)
            nodes[$-1].tree = toLong(func[0..index]);

        // number of calls
        nodes[$-1].func = toLong(Util.trim(func[index..$]));
    }

    CallNode[] nodes;
}


Words words(char[] str)
{
    return Words.iter(str);
}

struct Words
{
    private char[] src;

    static Words iter(char[] str)
    {
        Words result;
        result.src = str;
        return result;
    }

    int opApply (int delegate (ref char[] token) dg)
    {
        int result;
        uint start = 0;
        enum : ubyte
        {
            WORD,
            SPACE
        }
        ubyte state = SPACE;

        char[]  token;

        foreach (index, elem; src)
        {
            if(state == SPACE)
            {
                if (!Util.isSpace(elem))
                {
                    state = WORD;
                    start = index;
                }
            }
            else if (Util.isSpace(elem))
            {
                token = src[start..index];
                dg(token);
                state = SPACE;
            }

        }
        if (state == WORD && start < src.length)
        {
            token = src[start..$];
            dg(token);
        }
        return result;
    }
}

class ParseException : Exception
{
    this(char[] msg)
    {
        super(msg);
    }
}

class TraceReader
{
    this(char[] filename, TraceParser parser)
    {
        this.fileName = filename;
        this.parser = parser;
    }

    void read()
    {
        timings.length = 0;
        auto contents = new FileConduit(fileName);
        auto lines = new LineIterator!(char)(contents);
        bool pastGraph = false;
        char[] line;

        if (lines.next)
        {
            line = Util.trim(lines.get);
            if (line.length)
            {
                if (line[0] != '-')
                    throw new ParseException("unexpected format of trace log file");
            }
            else
                throw new ParseException("empty file");
        }

        char[][]    fanInLines;
        char[][]    fanOutLines;
        char[]      callLine;
        bool        pastCall = false;
        uint        count = 0;


        while(lines.next && !pastGraph)
        {
            count++;

            line = Util.trim(lines.get);

            if(!line.length)
                continue;
            else if(line[0] == '-' || line[0] == '=')
            {
                parser.processCall(fanInLines, callLine, fanOutLines);
                fanInLines.length = 0;
                fanOutLines.length = 0;
                callLine.length = 0;
                pastCall = false;
                if (line[0] == '=')
                    pastGraph = true;
                else
                    continue;
            }
            else
            {
                if (line[0] == '_')
                {
                    pastCall = true;
                    callLine = line.dup;
                }
                else if(!pastCall) // fan_in
                    fanInLines ~= line.dup;
                else if(pastCall) // fan_Out
                    fanOutLines ~= line.dup;
            }
        }

        auto freqString = Regex.search(line, r"[\d]+").match(0);
        if(freqString !is null)
            freq = ConvertFloat.toFloat(freqString) / 1000000.0;

        if (lines.next())
            line = lines.get;

        char[][]    values;

        while(lines.next)
        {
            values.length = 0;
            foreach(inout word; (lines.get()).words())
                values ~= word.dup;

            if (values.length == 5)
                timings ~= Timing(values[4].dup, toInt(values[0]), toInt(values[1]), toInt(values[2]), toInt(values[3]));
        }
    }

    /+char[] demangleSymbol(char[] mangledName)
    {
        auto ptr = mangledName in symbolTable;

        if (ptr)
            return *ptr;
        char[] result;
        try
        {
            result = demangle(mangledName);
            symbolTable[mangledName] = result;
        }
        catch(Object error)
        {
            Stdout("demangle error (ignored): ")(error.toString).newline;
            return mangledName;
        }

        return result;

    }+/
    alias getFQN demangleSymbol;


     //TODO: catch appropiate exception to not choke on invalid string-8 symbols but instead ignore them
    char[] getFQN(char[] mangledName)
    {
        return cachedLookup(mangledName, fqnMap, {
            fqnMap[mangledName] = demangle(mangledName);
            return fqnMap[mangledName];
        });
    }

    char[] getSYM(char[] mangledName)
    {
        return cachedLookup(mangledName, symMap, {
            char[] s;
            char[] t;
            s = demangle(mangledName, t);
            symMap[mangledName] = s;
            typeMap[mangledName] = t;
            return s;
        });
    }

    char[] getTYPE(char[] mangledName)
    {
        return cachedLookup(mangledName, typeMap, {
            char[] s;
            char[] t;
            s = demangle(mangledName, t);
            symMap[mangledName] = s;
            typeMap[mangledName] = t;
            return t;
        } );
    }

    private
    {
        char[] cachedLookup(char[] key, ref char[][char[]] map,  char[] delegate() create)
        {
            auto ptr = key in map;
            if (ptr)
                return *ptr;
            return create();
        }

        real        freq = 0;
        char[]      fileName;
        TraceParser parser;
        Timing[]    timings;
        char[][char[]] fqnMap;
        char[][char[]] symMap;
        char[][char[]] typeMap;
    }
}

private class MangleException : Exception
{
    this()
    {
        super("MangleException");
    }
}

const char[] macros =
`
macros:
TABLE = <table border="4" rules="none" cellpadding="6" cellspacing="6">$0</table>
TDR = <td align="right">$0</td>
TD_CSPAN = <td colspan="$1">$+</td>

TIMINGS = <h1> Timings (in microseconds) </h1>$(TABLE $0)
TIMING = <tr>$0</tr>
FUNCTION_NAME = <td>$0</td>
CALLS = <td align="right">$0</td>
TREE = <td align="right">$0</td>
FUNC = <td align="right">$0</td>
CALL = <td align="right">$0</td>

HEADER_FUNCTION_NAME = <th>$0</th>
HEADER_CALLS = <th>$0</th>
HEADER_TREE = <th>$0</th>
HEADER_FUNC = <th>$0</th>
HEADER_CALL = <th>$0</th>

CALL_GRAPH =
        <br>
        <br>
        $(P <h1> Call Graph, times are in microseconds</h1>
        $(TABLE

        <tr><td>Functions that call <i>measured function</i> (fan in)</td>
           <td>Num calls</td>
        </tr>
        <tr><td colspan="2"><b>Measured function symbol</b></td>
           <td align="right">Tree Time</td>
           <td align="right">Func Time</td>
           <td align="right">Num Calls</td>
        </tr>
        <tr><td>Functions called by <i>measured function</i> (fan out)</td>
           <td>Num calls</td>
        </tr>
        $(CALL_SEP)
        $0) )
CALL_SEP = <tr><td>&nbsp;</td></tr>
CALL_FUN = <tr>$0</tr>
CALL_FUN_NAME = $(TD_CSPAN $1, $(B $+))
CALL_FUN_TREE = <td align="right">$0</td>
CALL_FUN_FUNC = <td align="right">$0</td>
CALL_FUN_CALLS = <td align="right">$0</td>
FAN =  <tr>$0</tr>
FAN_FUNC = <td>$0</td>
FAN_CALLS = <td>$0</td>

DDOC =  <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
        <html><head>
        <META http-equiv="content-type" content="text/html; charset=utf-8">
        <link rel="stylesheet" type="text/css" href="style.css">
        </head><body>
        $(BODY)
        </body></html>
`;

char[] demangle(char[] name)
{
    char[] dummy;
    return demangle(name, dummy, ParseMode.Full);
}

/* This function is taken from phobos and ported to tango.*/
char[] demangle(char[] name, ref char[] type, ParseMode pMode = ParseMode.QualifiedName)
{

    size_t ni = 2;
    char[] delegate() fparseTemplateInstanceName;

    static void error()
    {
        throw new MangleException();
    }

    static ubyte ascii2hex(char c)
    {
    if (!isxdigit(c))
        error();
    return cast(ubyte)
          ( (c >= 'a') ? c - 'a' + 10 :
            (c >= 'A') ? c - 'A' + 10 :
                 c - '0'
          );
    }

    size_t parseNumber()
    {
        size_t result;

        while (ni < name.length && isdigit(name[ni]))
        {
            int i = name[ni] - '0';
            if (result > (size_t.max - i) / 10)
            error();
            result = result * 10 + i;
            ni++;
        }
        return result;
    }

    char[] parseSymbolName()
    {
        size_t i = parseNumber();
        if (ni + i > name.length)
            error();
        char[] result;
        if (i >= 5 &&
            name[ni] == '_' &&
            name[ni + 1] == '_' &&
            name[ni + 2] == 'T')
        {
            size_t nisave = ni;
            bool err;
            ni += 3;
            try
            {
                result = fparseTemplateInstanceName();
                if (ni != nisave + i)
                    err = true;
            }
            catch (MangleException me)
            {
                err = true;
            }
            ni = nisave;
            if (err)
                goto L1;
            goto L2;
        }
          L1:
            result = name[ni .. ni + i];
          L2:
            ni += i;
        return result;
    }

    char[] parseQualifiedName()
    {
        char[] result;

        while (ni < name.length && isdigit(name[ni]))
        {
            if (result.length)
                result ~= ".";
            result ~= parseSymbolName();
        }
        return result;
    }

    char[] parseType(char[] identifier = null)
    {
        int isdelegate = 0;
        bool hasthisptr = false; /// For function/delegate types: expects a 'this' pointer as last argument
          Lagain:
            if (ni >= name.length)
                error();
        char[] p;
        switch (name[ni++])
        {
            case 'v':	p = "void";	goto L1;
            case 'b':	p = "bool";	goto L1;
            case 'g':	p = "byte";	goto L1;
            case 'h':	p = "ubyte";	goto L1;
            case 's':	p = "short";	goto L1;
            case 't':	p = "ushort";	goto L1;
            case 'i':	p = "int";	goto L1;
            case 'k':	p = "uint";	goto L1;
            case 'l':	p = "long";	goto L1;
            case 'm':	p = "ulong";	goto L1;
            case 'f':	p = "float";	goto L1;
            case 'd':	p = "double";	goto L1;
            case 'e':	p = "real";	goto L1;
            case 'o':	p = "ifloat";	goto L1;
            case 'p':	p = "idouble";	goto L1;
            case 'j':	p = "ireal";	goto L1;
            case 'q':	p = "cfloat";	goto L1;
            case 'r':	p = "cdouble";	goto L1;
            case 'c':	p = "creal";	goto L1;
            case 'a':	p = "char";	goto L1;
            case 'u':	p = "wchar";	goto L1;
            case 'w':	p = "dchar";	goto L1;

            case 'A':               // dynamic array
            p = parseType() ~ "[]";
            goto L1;

//            case 'M':               // needs this*
//            p = parseType() ~ " class";
//            goto L1;

            case 'P':               // pointer
            p = parseType() ~ "*";
            goto L1;

            case 'G':               // static array
            {   size_t ns = ni;
            parseNumber();
            size_t ne = ni;
            p = parseType() ~ "[" ~ name[ns .. ne] ~ "]";
            goto L1;
            }

            case 'H':               // associative array
            p = parseType();
            p = parseType() ~ "[" ~ p ~ "]";
            goto L1;

            case 'D':               // delegate
            isdelegate = 1;
            goto Lagain;

            case 'M':
            hasthisptr = true;
            goto Lagain;

            case 'F':               // D function
            case 'U':               // C function
            case 'W':               // Windows function
            case 'V':               // Pascal function
            case 'R':               // C++ function
            {   char mc = name[ni - 1];
            char[] args;

            while (1)
            {
                if (ni >= name.length)
                error();
                char c = name[ni];
                if (c == 'Z')
                break;
                if (c == 'X')
                {
                    args ~= " ...";
                break;
                }
                if (args.length)
                args ~= ", ";
                switch (c)
                {
                case 'J':
                    args ~= "out ";
                    ni++;
                    goto default;

                case 'K':
                    args ~= "inout ";
                    ni++;
                    goto default;

                case 'L':
                    args ~= "lazy ";
                    ni++;
                    goto default;

                default:
                    args ~= parseType();
                    continue;

                case 'Y':
                    args ~= "...";
                    break;
                }
                break;
            }
            if (hasthisptr || isdelegate)
            {
                // add implicit 'this'/context pointer
                if (args.length)
                args ~= ", ";
                args ~= "void*";
            }
            ni++;
            if (!isdelegate && identifier.length)
            {
                switch (mc)
                {
                    case 'F': p = null;                break; // D function
                    case 'U': p = "extern (C) ";       break; // C function
                    case 'W': p = "extern (Windows) "; break; // Windows function
                    case 'V': p = "extern (Pascal) ";  break; // Pascal function
                    default:  assert(0);
                }
                p ~= parseType() ~ " " ~ identifier ~ "(" ~ args ~ ")";
                return p;
            }
            p = parseType() ~
                (isdelegate ? " delegate(" : " function(") ~
                args ~
                ")";
            isdelegate = 0;
            goto L1;
            }

            case 'C':   p = "class ";   goto L2;
            case 'S':   p = "struct ";  goto L2;
            case 'E':   p = "enum ";    goto L2;
            case 'T':   p = "typedef "; goto L2;

            L2: p ~= parseQualifiedName();
                goto L1;

            L1:
                if (isdelegate)
                    error();        // 'D' must be followed by function
                if (identifier.length)
                    p ~= " " ~ identifier;
                return p;

            default:
                size_t i = ni - 1;
                ni = name.length;
                p = name[i .. length];
                goto L1;
        }
        assert(0);
    }

    char[] parseTemplateInstanceName()
    {
        char[] result = parseSymbolName() ~ "!(";
        int nargs;

        while (1)
        {   size_t i;

            if (ni >= name.length)
                error();
            if (nargs && name[ni] != 'Z')
                result ~= ", ";
            nargs++;
            switch (name[ni++])
            {
                case 'T':
                    result ~= parseType();
                    continue;

                case 'V':

                    void getReal()
                    {
                        real r;
                        ubyte *p = cast(ubyte *)&r;

                        if (ni + 10 * 2 > name.length)
                            error();
                        for (i = 0; i < 10; i++)
                        {
                            ubyte b;

                            b = cast(ubyte)
                                (
                                    (ascii2hex(name[ni + i * 2]) << 4) +
                                    ascii2hex(name[ni + i * 2 + 1])
                                );
                            p[i] = b;
                        }
                    result ~= ConvertFloat.toString(r);
                    ni += 10 * 2;
                    }

                    result ~= parseType() ~ " ";
                    if (ni >= name.length)
                    error();
                    switch (name[ni++])
                    {
                    case '0': case '1': case '2': case '3': case '4':
                    case '5': case '6': case '7': case '8': case '9':
                        i = ni - 1;
                        while (ni < name.length && isdigit(name[ni]))
                        ni++;
                        result ~= name[i .. ni];
                        break;

                    case 'N':
                        i = ni;
                        while (ni < name.length && isdigit(name[ni]))
                        ni++;
                        if (i == ni)
                        error();
                        result ~= "-" ~ name[i .. ni];
                        break;

                    case 'n':
                        result ~= "null";
                        break;

                    case 'e':
                        getReal();
                        break;

                    case 'c':
                        getReal();
                        result ~= '+';
                        getReal();
                        result ~= 'i';
                        break;

                    case 'a':
                    case 'w':
                    case 'd':
                    {   char m = name[ni - 1];
                        if (m == 'a')
                        m = 'c';
                        size_t n = parseNumber();
                        if (ni >= name.length || name[ni++] != '_' ||
                        ni + n * 2 > name.length)
                        error();
                        result ~= '"';
                        for (i = 0; i < n; i++)
                        {   char c;

                        c = (ascii2hex(name[ni + i * 2]) << 4) +
                             ascii2hex(name[ni + i * 2 + 1]);
                        result ~= c;
                        }
                        ni += n * 2;
                        result ~= '"';
                        result ~= m;
                        break;
                    }

                    default:
                        error();
                        break;
                    }
                    continue;

                case 'S':
                    result ~= parseSymbolName();
                    continue;

                case 'Z':
                    break;

                default:
                    error();
            }
                break;
        }
        result ~= ")";
        return result;
    }

    if (name.length < 3 ||
        name[0] != '_' ||
        name[1] != 'D' ||
        !isdigit(name[2]))
    {
        goto Lnot;
    }

    fparseTemplateInstanceName = &parseTemplateInstanceName;

    try
    {
        /+
        auto result = parseQualifiedName();

        result = parseType(result);

        while(ni < name.length)
        {
            result ~= " . " ~ parseType(parseQualifiedName());
        }

        if (ni != name.length)
            goto Lnot;
        return result;
        +/

        if (pMode == ParseMode.Full)
        {
            auto result = parseQualifiedName();


            result = parseType(result);

            while(ni < name.length)
            {
                result ~= " . " ~ parseType(parseQualifiedName());
            }

            if (ni != name.length)
                goto Lnot;
            return result;
        }
        else
        {
            auto result = parseQualifiedName();


            type ~= parseType();

            while(ni < name.length)
            {
                result ~= " . " ~ parseQualifiedName();
                type ~= " . " ~ parseType();
            }

            if (ni != name.length)
                goto Lnot;
            return result;
        }
    }
    catch (MangleException e)
    {
    }


Lnot:
    // Not a recognized D mangled name; so return original
    return name;
}
