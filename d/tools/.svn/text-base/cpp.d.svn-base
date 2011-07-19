module tools.cpp;

import tools.base;

alias tools.base.Ret Ret;
alias tools.base.Params Params;

private template Select(alias Cond, T...) {
  static if (!T.length) alias Tuple!() Select;
  else static if (Cond!(T[0])) alias Tuple!(T[0], Select!(Cond, T[1..$])) Select;
  else alias Select!(Cond, T[1..$]) Select;
}

private template SelectOne(alias Cond, T...) {
  static if (!T.length) static assert(false, "Cannot select via "~Cond.stringof);
  else static if (Cond!(T[0])) alias T[0] SelectOne;
  else alias SelectOne!(Cond, T[1..$]) SelectOne;
}

private template HasOne(alias Cond, T...) {
  static if (!T.length) const bool HasOne = false;
  else static if (Cond!(T[0])) const bool HasOne = true;
  else const bool HasOne = HasOne!(Cond, T[1..$]);
}

private template SelectOneParameterized(alias Cond, Param, T...) {
  static if (!T.length) static assert(false, "Cannot select one via "~Cond.stringof);
  else static if (Cond!(Param.Tuple, T[0])) alias T[0] SelectOneParameterized;
  else alias SelectOneParameterized!(Cond, Param, T[1..$]) SelectOneParameterized;
}

private template SelectIndexParameterized(alias Cond, Param, T...) {
  static if (!T.length) static assert(false, "Cannot select one via "~Cond.stringof);
  else static if (Cond!(Param.Tuple, T[0])) const int SelectIndexParameterized = 0;
  else const int SelectIndexParameterized = SelectIndexParameterized!(Cond, Param, T[1..$]) + 1;
}

private template HasOneParameterized(alias Cond, Param, T...) {
  static if (!T.length) const bool HasOneParameterized = false;
  else static if (Cond!(Param.Tuple, T[0])) const bool HasOneParameterized = true;
  else const bool HasOneParameterized = HasOneParameterized!(Cond, Param, T[1..$]);
}

static if (size_t.sizeof == 4) {
  alias int C_int;
  alias uint C_uint;
  alias int C_long;
  alias uint C_ulong;
} else static if (size_t.sizeof == 8) {
  alias int C_int;
  alias uint C_uint;
  alias long C_long;
  alias ulong C_ulong;
}

template derefPointer(T: T*) { alias T derefPointer; }

template isFunction(T) {
  const bool isFunction = is(Ref!(T)) && is(Params!(T));
}

template TypeMatches(S, T) { const bool TypeMatches = is(S == T); }

template CppMangle(Substs, T) {
  // only substitute "long" types
  static if ((is(typeof(T.Name): string) || isPointer!(T)) && HasOneParameterized!(TypeMatches, TW!(T), Substs.Tuple)) {
    // pragma(msg, "backref to ", T.stringof, " in ", Substs.stringof, " at ", ctToString(SelectIndexParameterized!(TypeMatches, TW!(T), Substs.Tuple)));
    /*static if (!SelectIndexParameterized!(TypeMatches, TW!(T), Substs.Tuple)) {
      const string CppMangle = "S_";
    } else {*/
      const string CppMangle = "S"~ctToString(SelectIndexParameterized!(TypeMatches, TW!(T), Substs.Tuple))~"_";
    /*}*/
  }
  else static if (is(T == void)) const string CppMangle = "v";
  else static if (is(T == wchar)) const string CppMangle = "w";
  else static if (is(T == bool)) const string CppMangle = "b";
  else static if (is(T == char)) const string CppMangle = "c";
  else static if (is(T == byte)) const string CppMangle = "a";
  else static if (is(T == ubyte)) const string CppMangle = "h";
  else static if (is(T == short)) const string CppMangle = "s";
  else static if (is(T == ushort)) const string CppMangle = "t";
  else static if (is(T == long)) const string CppMangle = "x";
  else static if (is(T == ulong)) const string CppMangle = "y";
  else static if (is(T == C_int)) const string CppMangle = "i";
  else static if (is(T == C_uint)) const string CppMangle = "j";
  else static if (is(T == C_long)) const string CppMangle = "l";
  else static if (is(T == C_ulong)) const string CppMangle = "m";
  else static if (is(T == float)) const string CppMangle = "f";
  else static if (is(T == double)) const string CppMangle = "d";
  else static if (is(T == real)) const string CppMangle = "e";
  else static if (isPointer!(T)) const string CppMangle = "P" ~ CppMangle!(Substs, derefPointer!(T));
  else static if (isFunction!(T)) const string CppMangle =
    "F" ~ CppMangle!(Substs, Ret!(T))~CppMangleParams!(Refs!(T), true, Params!(T)) ~ "E";
  else static if (is(T.isConst)) {
    static if (is(typeof(T.Name): string))
      const string CppMangle = "K"~ctToString(T.Name.length)~T.Name;
    else
      const string CppMangle = "K"~CppMangle!(Substs, typeof(T.value));
  } else static if (is(typeof(T.Name) : string)) {
      const string CppMangle = ctToString(T.Name.length)~T.Name;
  } else {
    pragma(msg, "Type "~T.stringof~" unmangled. ");
    const string CppMangle = "unknown";
  }
}

// I shall do science to it!
template DoScience(T, char Ref) {
  static if (isPointer!(T) && !is(derefPointer!(T) == void))
    alias Tuple!(DoScience!(derefPointer!(T), Ref), T) DoScience;
  else static if (is(T.isConst))
    alias Tuple!(T.Type, Repeat!(Filler, Ref == 't'), T) DoScience;
  else
    alias Tuple!(Repeat!(Filler, Ref == 't'), T) DoScience;
}

template CppMangleParams(string Refs, bool First, Substs, T...) {
  static if (!T.length) const string CppMangleParams = First?"v":"";
  else static if (CppMangle!(Substs, T[0])[0] == 'S') 
    const string CppMangleParams =
      CppMangle!(Substs, T[0]) ~
      CppMangleParams!(Refs[1 .. $], false, TW!(Substs.Tuple, DoScience!(T[0], Refs[0])), T[1 .. $]);
  else
    const string CppMangleParams =
      (Refs[0]=='t'?"R":"") ~
      CppMangle!(Substs, T[0]) ~
      CppMangleParams!(Refs[1 .. $], false, TW!(Substs.Tuple, DoScience!(T[0], Refs[0])), T[1 .. $]);
}

struct Filler { } // manually pad the substitution cache

template FunMangle(string Name, T) {
  static if (Name.length > 4 && Name[0 .. 4] == "this") {
    const string FunMangle = "C"~Name[4 .. $]
      ~"E"~CppMangleParams!(isRef!(T)[1..$], true, TW!(), Params!(T)[1..$]);
  } else {
    const string FunMangle =  
      ctToString(Name.length)~Name
      //                         skip 'this' pointer
      ~"E"~CppMangleParams!(isRef!(T)[1..$], true, TW!(), Params!(T)[1..$]);
  }
}

alias TupleWrapper TW;

// bind to the G++ ABI

struct VirtualMethod(alias _Fn, string _Name) {
  alias _Fn Fn;
  const value = &Fn;
  const string Name = _Name;
  alias void isVirtual;
  alias void isMethod;
}

Ret!(T) abstr(T, string Name)(Params!(T) params) {
  throw new Exception(Name~" not implemented! ");
}

struct AbstractMethod(T, string _Name) {
  alias abstr!(T, _Name) Fn;
  const value = &Fn;
  const string Name = _Name;
  alias void isVirtual;
}

struct Method(alias _Fn, string _Name) {
  alias _Fn Fn;
  const value = &Fn;
  alias typeof(value) Type;
  const string Name = _Name;
  alias void isMethod;
}

template thunkcall(int D, alias Sup) {
  mixin(`Ret!(typeof(&Sup))
    call(`~refToParamList("Params!(typeof(&Sup))", isRef!(typeof(&Sup)))~`) {
      param_0 = cast(typeof(param_0)) (cast(void*) param_0 - D);
      return Sup(`~refToValueList(isRef!(typeof(&Sup)))~`);
    }`);
}

struct ThunkedMethod(alias _Fn, int Offs, string _Name) {
  alias thunkcall!(Offs, _Fn).call Fn;
  const value = &Fn;
  const string Name = _Name;
  mixin(`const string Mangle="`~Name~`_`~ctToString(Offs)~`_?"; `);
  alias void isMethod;
}

struct Member(T, string _Name, string _Init = "") {
  const string Name = _Name;
  const string Init = _Init;
  static T value; // bogus to work around a gdc bug
  alias T Type;
  alias void isMember;
}

template MemberVars(T...) {
  static if (T.length) {
    static if (T[0].Init.length)
      mixin("typeof(T[0].value) "~T[0].Name~" = "~T[0].Init~"; ");
    else mixin("typeof(T[0].value) "~T[0].Name~"; ");
    // yes the extra check is necessary
    // no don't ask me why
    static if (T.length > 1) mixin MemberVars!(T[1..$]);
  }
}

template isVirtual(T) { const bool isVirtual = is(T.isVirtual); }
template isMethod(T) { const bool isMethod = is(T.isMethod); }
template isMethodNotVirtual(T) { const bool isMethodNotVirtual = isMethod!(T) && !isVirtual!(T); }
template isMember(T) { const bool isMember = is(T.isMember); }
template isClass(T) { const bool isClass = is(T.ClassMarker); }
template isVirtualClass(T) { const bool isVirtualClass = isClass!(T) && is(typeof(T.vtable)); }
// T is in some way related to vtables
template stuffVTable(T) { const bool stuffVTable = isVirtual!(T) || isVirtualClass!(T); }

template isName(string S, T) {
  static if (is(T.CppClass)) const bool isName = false;
  else const bool isName = T.Name == S;
}

import tools.ctfe;
string vtableGen(int len, bool type) {
  string res = "";
  for (int i = 0; i < len; ++i) {
    
    if (type) res ~= `
      static if (is(typeof(Virtuals[!!].isClassInfo))) {
        mixin("typeof(Virtuals[!!].value) classinfo_" ~ Virtuals[!!].Name ~ "; ");
        // pragma(msg, "!! typeof(Virtuals[!!].value) classinfo_" ~ Virtuals[!!].Name ~ "; ");
      } else {
        mixin("typeof(Virtuals[!!].value) " ~ Virtuals[!!].Name ~ "_!!; ");
        // pragma(msg, "!! typeof(Virtuals[!!].value) " ~ Virtuals[!!].Name ~ "_!!; ");
      }
      // static if (is(typeof(Virtuals[!!].Fn))) pragma(msg, "being ", Virtuals[!!].value.stringof);
    `.ctReplace("!!", ctToString(i));
    else {
      if (res.length) res ~= ", ";
      res ~= "Virtuals[!].value".ctReplace("!", ctToString(i));
    }
  }
  if (type) return res;
  else return "{"~res~"}";
}

struct VTableType(Virtuals...) {
  // pragma(msg, "VTableType: ", vtableGen(Virtuals.length, true), " for ", Virtuals.stringof);
  mixin(vtableGen(Virtuals.length, true));
}

template VTableData(Virtuals...) {
  mixin("VTableType!(Virtuals) data = "~vtableGen(Virtuals.length, false)~"; ");
}

string effs(int i) {
  string res;
  while (i--)
    res ~= 'f';
  return res;
}

string enable(int id, string s) {
  s[id] = 't';
  return s;
}

string unify(string s, string t) {
  assert(s.length == t.length);
  for (int i = 0; i < s.length; ++i) {
    if (t[i] == 't')
      s[i] = 't';
  }
  return s;
}

template UpdateWithThunks(int Offset, VTable, Members...) {
  // pragma(msg, "UWT: members ", Members.stringof, " to fix up vtable ", VTable.stringof);
  static if (!VTable.Tuple.length) {
    alias Tuple!() thunks;
    const string mask = effs(Members.length);
   } else {
    static if (HasOneParameterized!(isName, TW!(VTable.Tuple[0].Name), Members)) {
      alias Tuple!(ThunkedMethod!(
        SelectOneParameterized!(isName, TW!(VTable.Tuple[0].Name), Members).Fn,
        Offset,
        VTable.Tuple[0].Name
      ), UpdateWithThunks!(Offset, TW!(Tuple!(VTable.Tuple)[1 .. $]), Members).thunks) thunks;
      const string mask = enable(
        SelectIndexParameterized!(isName, TW!(VTable.Tuple[0].Name), Members),
        UpdateWithThunks!(Offset, TW!(Tuple!(VTable.Tuple)[1 .. $]), Members).mask
      );
    } else {
      alias Tuple!(
        Tuple!(VTable.Tuple[0])[0],
        Tuple!(UpdateWithThunks!(Offset, TW!(Tuple!(VTable.Tuple)[1 .. $]), Members).thunks)
      ) thunks;
      const string mask = UpdateWithThunks!(Offset, TW!(Tuple!(VTable.Tuple)[1 .. $]), Members).mask;
    }
  }
}

struct SuppressClassinfo { alias void suppressClassinfo; }
template isSuppr(T) { const bool isSuppr = is(T.suppressClassinfo); }

template useClassinfo(T...) { const bool useClassinfo = !Select!(isSuppr, T).length; }

struct Classinfo(string _Name) {
  const string Name = _Name;
  alias void* Type;
  const void* value = null;
  const isClassInfo = true;
}

// fill in virtuals with thunk
template FixSuperClass(Class, int Offset, int TableOffset, VTable, Members...) {
  static assert(TableOffset != -1);
  alias Class.Virtuals Virtuals;
  // pragma(msg, "FixSuperClass(", Class.Name, "; vtable ", VTable.stringof, " at offset ", TableOffset.stringof, ")");
  static if (Class.hasVTable) {
    mixin(`void* vtable_`~Class.Name~` =
      cast(void*) &VTableData!(VTable.Tuple).data.classinfo_`~Class.Name~` + 4;
      alias typeof(VTableData!(VTable.Tuple).data) VTable_`~Class.Name~`;
    `);
  }
  mixin CppClassMembers!(
    Class.hasVTable * 4 + Offset,
    Class.hasClassinfo * 4 + TableOffset,
    TW!(Class.Members),
    TW!(Members),
    VTable
  );
  mixin(`Class* cast_`~Class.Name~`() { return cast(Class*) (cast(void*) this + `~ctToString(Offset)~`); } `);
  mixin(`const offset_`~Class.Name~` = `~ctToString(Offset)~`; `);
  static if (Class.hasClassinfo)
    mixin(`static void fixup_classinfo_`~Class.Name~`(void* p) {
      VTableData!(VTable.Tuple).data.classinfo_`~Class.Name~` = p;
    }`);
  // pragma(msg, `Class* cast_`~Class.Name~`() { return cast(Class*) (cast(void*) this + `~ctToString(Offset)~`); } `);
}

struct Str(alias A, P...) {
  mixin A!(P);
}

template Size(alias A, P...) {
  const int Size = Init!(Str!(A, P)).sizeof;
}

template SuperClasses(Members, int DataOffs, int TableOffs, VTable, Classes...) {
  static if (Classes.length) {
    mixin FixSuperClass!(Classes[0], DataOffs, TableOffs, VTable, Members.Tuple);
    mixin SuperClasses!(
      Members,
      DataOffs + Size!(FixSuperClass, Classes[0], DataOffs, TableOffs, VTable, Members.Tuple),
      TableOffs + Classes[0].FixedVTable.length * 4 + Classes[0].hasClassinfo * 4,
      VTable, Classes[1 ..$]
    );
  }
}

template CppClassMembers(int DataOffs, int VTableOffs, UsedMembers, AllMembers, VTable) {
  mixin SuperClasses!(UsedMembers, DataOffs, VTableOffs, VTable, Select!(isClass, UsedMembers.Tuple));
  mixin MemberVars!(Select!(isMember, UsedMembers.Tuple));
}

template DeepSuperNames(Members...) {
  static if (!Members.length) const string DeepSuperNames = "";
  else {
    static if (isClass!(Tuple!(Members)[0])) {
      static if (is(typeof(Tuple!(Members)[0].vtable)))
        const string DeepSuperNames = 
              Members[0].Name
         ~","~DeepSuperNames!(Tuple!(Members)[0].Members)
         ~","~DeepSuperNames!(Tuple!(Members)[1 .. $]);
      else
        const string DeepSuperNames =
              DeepSuperNames!(Tuple!(Members)[0].Members)
         ~","~DeepSuperNames!(Tuple!(Members)[1 .. $]);
    } else const string DeepSuperNames = DeepSuperNames!(Tuple!(Members)[1 .. $]);
  }
}

template FirstClass(Members...) {
  static if (!Members.length) alias Tuple!() FirstClass;
  else static if (isClass!(Tuple!(Members)[0])) {
    static if (FirstClass!(Members[0].Members).length) alias FirstClass!(Members[0].Members) FirstClass;
    else alias Tuple!(Members[0]) FirstClass;
  } else alias FirstClass!(Members[1 .. $]) FirstClass;
}

string gen_fixups(string basename, string param, string bases) {
  string res = "";
  while (bases.length) {
    auto base = bases.ctSlice(",");
    if (!base.length) continue;
    res ~= "static if (is(typeof(&"~basename~"_"~base~")))
      "~basename~"_"~base~"("~param~"); ";
  }
  return res;
}

template CombineUpdateVTables(Members, int Offset, Classes...) {
  static if (!Classes.length) {
    alias Tuple!() table;
    const string mask = effs(Members.Tuple.length);
  } else {
    // pragma(msg, "[] ", Offset.stringof, ", ", Classes[0].Name, ", ", Classes[0].FixedVTable.stringof);
    alias Tuple!(
      UpdateWithThunks!(Offset, TW!(Classes[0].FixedVTable), Members.Tuple).thunks,
      CombineUpdateVTables!(Members, Offset + Classes[0].sizeof, Classes[1 .. $]).table
    ) table;
    const string mask = unify(
      UpdateWithThunks!(Offset, TW!(Classes[0].FixedVTable), Members.Tuple).mask,
      CombineUpdateVTables!(Members, Offset + Classes[0].sizeof, Classes[1 .. $]).mask
    );
  }
}

template GenVirtualCalls(T, FixedVTable, int Offs) {
  static if (T.Tuple.length) {
    // pragma(msg, ":virtual:", T.Tuple[0].Name);
    static if (Params!(typeof(T.Tuple[0].value)).length > 1) {
      mixin(`Ret!(typeof(T.Tuple[0].value)) `~T.Tuple[0].Name~`(`
        ~refToParamList(`Params!(typeof(T.Tuple[0].value))[1..$]`, isRef!(typeof(T.Tuple[0].value))[1..$])~`) {
        // the vtable member is omitted when it's present in a first superclass
        auto vtable = *cast(void**) this;
        auto vtd = cast(typeof(VTableData!(FixedVTable.Tuple).data)*) (vtable - 4);
        return vtd.`~T.Tuple[0].Name~`_`~ctToString(Offs)~`(
          this, `~refToValueList(isRef!(typeof(T.Tuple[0].value))[1..$])~`
        );
      }`);
    } else {
      mixin(`Ret!(typeof(T.Tuple[0].value)) `~T.Tuple[0].Name~`(`
        ~refToParamList(`Params!(typeof(T.Tuple[0].value))[1..$]`, isRef!(typeof(T.Tuple[0].value))[1..$])~`) {
        auto vtable = *cast(void**) this;
        auto vtd = cast(typeof(VTableData!(FixedVTable.Tuple).data)*) (vtable - 4);
        return vtd.`~T.Tuple[0].Name~`_`~ctToString(Offs)~`(this);
      }`);
    }
    mixin GenVirtualCalls!(TW!(T.Tuple[1 .. $]), FixedVTable, Offs + 1);
  }
}

template GenLocalCalls(T...) {
  static if (T.length) {
    // pragma(msg, ":local:", T[0].Name);
    static if (Params!(typeof(T[0].value)).length > 1) {
      mixin(`Ret!(typeof(T[0].value)) `~T[0].Name~`(`
        ~refToParamList(`Params!(typeof(T[0].value))[1..$]`, isRef!(typeof(T[0].value))[1..$])~`) {
        return T[0].Fn(
          this, `~refToValueList(isRef!(typeof(T[0].value))[1..$])~`
        );
      }`);
    } else {
      mixin(`Ret!(typeof(T[0].value)) `~T[0].Name~`(`
        ~refToParamList(`Params!(typeof(T[0].value))[1..$]`, isRef!(typeof(T[0].value))[1..$])~`) {
        return T[0].Fn(this);
      }`);
    }
    static if (T.length > 1)
      mixin GenLocalCalls!(T[1 .. $]);
  }
}

template MaskSelect(string S, T...) {
  static if (!T.length) {
    alias Tuple!() MaskSelect;
  } else {
    static if (S[0] == 'f') // not used to override
      alias Tuple!(T[0], MaskSelect!(S[1..$], T[1..$])) MaskSelect;
    else // overrid, omit
      alias Tuple!(MaskSelect!(S[1..$], T[1..$])) MaskSelect;
  }
}

extern(C) struct CppClass(string _Name, _Members...) {
  alias _Name Name;
  alias _Members Members;
  static if (HasOne!(isClass, Members)) {
    const bool hasVTable = HasOne!(stuffVTable, Members)
      && !FirstClass!(Select!(isClass, Members))[0].hasVTable;
    const bool hasClassinfo = !SelectOne!(isClass, Members).hasVTable;
  } else {
    const bool hasVTable = HasOne!(stuffVTable, Members);
    const bool hasClassinfo = true;
  }
  
  alias CombineUpdateVTables!(TW!(Select!(isMethod, Members)), hasVTable * 4, Select!(isClass, Members)).table ParentalVTable;
  const MethodMask = CombineUpdateVTables!(TW!(Select!(isMethod, Members)), hasVTable * 4, Select!(isClass, Members)).mask;
  alias Select!(isVirtual, MaskSelect!(MethodMask, Select!(isMethod, Members))) Virtuals;
  
  static if (hasClassinfo)
    alias Tuple!(Classinfo!(Name)) _CI;
  else alias Tuple!() _CI;
  alias Tuple!(_CI,
    ParentalVTable,
    Virtuals
  ) FixedVTable;
  const int OwnVtableOffset = _CI.length + ParentalVTable.length;
  mixin GenVirtualCalls!(TW!(Virtuals), TW!(FixedVTable), OwnVtableOffset);
  static if (hasVTable) {
    void* vtable = cast(void*) &VTableData!(FixedVTable).data + 4;
    // pragma(msg, "CppClass(", Name, ": vtable ", FixedVTable.stringof, ")");
    mixin(`static void fixup_classinfo_`~Name~`(void* p) {
      VTableData!(FixedVTable).data.classinfo_`~Name~` = p;
    }`);
    static void fixup_classinfo(void* p) {
      mixin(gen_fixups("fixup_classinfo", "p", Name~","~DeepSuperNames!(Members)));
    }
    T get_type_info(T)() {
      return *cast(T*) (vtable - 4);
    }
  } else {
    static void fixup_classinfo(void* p) {
      mixin(gen_fixups("fixup_classinfo", "p", DeepSuperNames!(Members)));
    }
    T get_type_info(T)() {
      // try to use the first eligible child's vtable instead
      static if (HasOne!(stuffVTable, Members)) {
        return mixin(`*cast(T*) (vtable_`~SuperOrder!(Members)[0].Name~` - 4)`);
      } else {
        static assert(false, "No VTable in "~Name);
      }
    }
  }
  mixin GenLocalCalls!(Select!(isMethodNotVirtual, Members));
  mixin CppClassMembers!(hasVTable * 4, hasClassinfo * 4, TW!(Members), TW!(Members), TW!(FixedVTable));
  alias void ClassMarker;
}

int readSymbolInt(string s, int i) {
  i = s.idxStripL(i);
  while (i < s.length) {
    char c = s[i];
    if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'
      || c == '_' || c == '.') {
      i++;
    } else {
      return s.idxStripL(i);
    }
  }
  assert(false, "String terminated by symbol. Oops. ");
}

string readSymbol(string s, ref int i) {
  auto start = i;
  i = s.readSymbolInt(i);
  return s[start .. i];
}

// read until closing " or `
string readString(ref string s, char marker) {
  auto backup = s;
  while (s.length) {
    if (s[0] == marker) {
      s = s[1 .. $];
      return backup[0 .. $ - s.length - 1];
    }
    if (s[0] == '\\' && marker == '"') s = s[1 .. $]; // skip
    s = s[1 .. $];
  }
  assert(false, "String literal not closed! ");
}

int readString(string s, int i, char marker) {
  while (i < s.length) {
    if (s[i] == marker) {
      return i + 1;
    }
    if (s[i] == '\\' && marker == '"') i++;
    i++;
  }
  assert(false, "String literal not closed! ");
}

void eatCComment(ref string s) {
  s.ctSlice("*/");
  assert(s, "Closing comment */ not found! ");
}

void eatDComment(ref string s) {
  int depth = 1;
  while (s.length) {
    size_t pos1 = s.ctFind("/+"), pos2 = s.ctFind("+/");
    assert(pos1 != pos2, "Closing comment +/ not found! ");
    if (pos1 < pos2) {
      s = s[pos1+2 .. $];
      depth++;
    } else {
      s = s[pos2+2 .. $];
      depth--;
      if (!depth) return;
    }
  }
  assert(false);
}

string eatComments(string s) {
  string res;
  int len;
  while (len + 1 < s.length) {
    auto ch = s[len];
    if (ch == '"' || ch == '`') {
      res ~= s[0 .. len]; s = s[len + 1 .. $]; len = 0;
      res ~= ch ~ s.readString(ch) ~ ch;
      continue;
    }
    if (s[len .. len + 2] == "/*") {
      res ~= s[0 .. len]; s = s[len + 2 .. $]; len = 0;
      s.eatCComment();
      continue;
    }
    if (s[len .. len + 2] == "/+") {
      res ~= s[0 .. len]; s = s[len + 2 .. $]; len = 0;
      s.eatDComment();
      continue;
    }
    if (s[len .. len + 2] == "//") {
      res ~= s[0 .. len]; s = s[len + 2 .. $]; len = 0;
      s.ctSlice("\n");
      continue;
    }
    len ++;
  }
  res ~= s[0 .. len + 1];
  return res;
}

int readBrackets(string s, int i, char open, char close) {
  int count = 1;
  assert(s[i] == open, "Expected "~open~"; found "~s);
  i++;
  while (i < s.length) {
    auto ch = s[i];
    if (ch == open) count ++;
    if (ch == close) count --;
    if (ch == '"' || ch == '`') {
      i = s.readString(i+1, ch);
      continue;
    }
    if (!count) {
      return i + 1;
    }
    i++;
  }
  assert(false, "Closing bracket not found in "~s~"!");
}

string readBracketsStr(string s, ref int i, char open, char close) {
  auto start = i;
  i = s.readBrackets(i, open, close);
  return s[start .. i];
}

int idxStripL(string s, int i) {
  while (i < s.length && (s[i] == ' ' || s[i] == '\t' || s[i] == '\n' || s[i] == '\r'))
    i++;
  return i;
}

string readType(string s, ref int i) {
  auto start = i;
  i = s.readSymbolInt(i);
  int failsafe;
  while (true) {
    failsafe ++;
    if (failsafe > 100) {
      assert(false, "Parser hanging? "~s);
    }
    i = s.idxStripL(i);
    if (i < s.length && s[i] == '!') {
      i++;
    }
    if (i < s.length && s[i] == '(') {
      i = s.readBrackets(i, '(', ')');
      continue;
    }
    if (i < s.length && s[i] == '*') {
      i++;
      continue;
    }
    if (i < s.length && s[i] == '[') {
      i = s.readBrackets(i, '[', ']');
      continue;
    }
    if (i < s.length - 8 && s[i .. i+8] == "function") {
      i = s.idxStripL(i+8);
      i = s.readBrackets(i, '(', ')');
      continue;
    }
    break;
  }
  return s[start .. i];
}

string readInitializer(string s, ref int i) {
  i = s.idxStripL(i);
  if (s[i] != '=') return "";
  i++;
  auto start = i;
  int pos1 = s[i .. $].ctFind(","), pos2 = s[i .. $].ctFind(";");
  if (pos1 == pos2) assert(false, "Initializer not closed in "~s);
  int sep = pos1;
  if (pos2 < sep) sep = pos2;
  i += sep;
  return s[start .. i];
}

string haaax(string s) {
  return s.eatComments().haaax_main();
}

string ctSlice(string s, ref int i, string m) {
  auto pos = s[i .. $].ctFind(m);
  assert(pos != -1);
  auto res = s[i .. i+pos];
  i += pos + m.length;
  return res;
}

string ctSlice(ref string s, string m) { return tools.ctfe.ctSlice(s, m); }

// parse a pseudocode C++ class declaration
string haaax_main(string s, int i = 0) {
  string res, fndefs;
  auto info = s.ctSlice(i, "{");
  auto name = info.ctSlice(":").ctStrip();
  info = info.ctStrip();
  res ~= "alias CppClass!(\""~name~"\",";
  while (info.length) {
    auto supclass = info.ctSlice(",");
    info = info.ctStrip();
    res ~= supclass~",";
  }
  i = s.idxStripL(i);
  int num = 0;
  while (i < s.length) {
    num ++;
    if (s[i] == '}') { i++; break; }
    bool virtual = false;
    if (i + 7 < s.length && s[i .. i+7] == "virtual") {
      i = s.idxStripL(i+7);
      virtual = true;
    }
    string mname;
    if (s[i .. i+8] == "mangleto") {
      i = s.idxStripL(i+8);
      mname = "\"" ~ s.readBracketsStr(i, '(', ')')[1 .. $-1] ~ "\"";
      i = s.idxStripL(i);
    }
    auto t = s.readType(i), n = s.readSymbol(i), init = s.readInitializer(i);
    i = s.idxStripL(i);
    if (s[i] == '(') {
      auto fnparams = s.readBracketsStr(i, '(', ')').ctStrip()[1 .. $-1].ctStrip();
      if (fnparams.length) fnparams = "(void *_self, "~fnparams~")";
      else fnparams = "(void *_self)";
      
      i = s.idxStripL(i);
      bool constfun = false;
      if (i + 5 < s.length && s[i .. i+5] == "const") {
        i = s.idxStripL(i + 5);
        constfun = true;
      }
      auto type = t~" function "~fnparams;
      if (!mname.length)
        mname = `"_ZN`~(constfun?"K":"")~ctToString(name.length)~name~`"~FunMangle!("`~n~`", `~type~`)`;
      auto nice_name = `__cpp_`~name~`_`~n~`_`~ctToString(num)~`__`;
      auto fn = t~` `~nice_name;
      bool exportExtern = false;
      if (s[i] == ';') {
        fndefs ~= `pragma(msg, `~mname~`);`~'\n';
        fndefs ~= `mixin("extern(C) `~t~` "~`~mname~`~"`~fnparams.ctReplace(`"`, `\"`)~`; ");`~'\n'~
          `mixin("`~fn~`("~refToParamList("Params!(typeof(&"~`~mname~`~"))", isRef!(typeof(mixin("&"~`~mname~`))))~") {
            return "~`~mname~`~"("~refToValueList(isRef!(typeof(mixin("&"~`~mname~`))))~");
          }
        "); `~'\n';
        res ~= (virtual?`VirtualMethod`:`Method`)~`!(`~nice_name~`, "`~n~`"),`;
        i ++;
      } else if (s[i] == '{') {
        fndefs ~= fn~fnparams~`{ auto self = cast(`~name~`*) _self; with (*self) ` ~ s.readBracketsStr(i, '{', '}')[0 .. $]~" } \n";
        exportExtern = true;
        res ~= (virtual?`VirtualMethod`:`Method`)~`!(`~nice_name~`, "`~n~`"),`;
      } else if (s[i] == '=') {
        i = s.idxStripL(i+1);
        assert(s[i] == '0', "Invalid semantics: virtual method = <anything but null> is not permissible! ");
        i = s.idxStripL(i+1);
        assert(s[i] == ';', "Missing semicolon after =0! ");
        i ++;
        fndefs ~= fn~fnparams~`{ throw new Exception("`~name~`::`~n~` not implemented! "); }`~'\n';
        res ~= `VirtualMethod!(`~nice_name~`, "`~n~`"),`;
        exportExtern = true;
      } else assert(false, "!! "~s[i .. $]);
      if (exportExtern)
        fndefs ~= `mixin("extern(C) `~t~` "~`~mname~`~"("~refToParamList("Params!(typeof(&`~nice_name~`))",
          isRef!(typeof(&`~nice_name~`)))~") {
            return `~nice_name~`("~refToValueList(isRef!(typeof(&`~nice_name~`)))~");
          }"); `~'\n';
    } else if (s[i] == ';') {
      i ++;
      res ~= `Member!(`~t~`, "`~n~`", "`~init~`"),`;
    } else if (s[i] == ',') {
      do {
        i ++;
        res ~= `Member!(`~t~`, "`~n~`", "`~init~`"),`;
        n = s.readSymbol(i);
        init = s.readInitializer(i);
        i = s.idxStripL(i);
      } while (s[i] == ',');
      assert(s[i] == ';', "Not terminated: "~s);
      i ++;
      res ~= `Member!(`~t~`, "`~n~`", "`~init~`"),`;
    } else assert(false, "Parser was confused by unexpected data: "~s[i .. $]);
    i = s.idxStripL(i);
  }
  i = s.idxStripL(i);
  res = res[0 .. $-1]; // eat trailing comma
  res ~= ") "~name~";\n";
  if (i < s.length) return fndefs ~ res ~ s.haaax_main(i);
  else return fndefs ~ res;
}
