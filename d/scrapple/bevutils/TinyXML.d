/***************************************************************
Copyright (c) Steve Teale 2007
This program is free software; you can use it for any purpose
subject to the following conditions.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.
****************************************************************/

module bevutils.tinyxml;

import std.regexp;
import std.string;
import std.stdio;
import std.stream;

alias std.string.find indexOf;
alias std.string.split split;
alias std.string.toString toString;

/**
 * A compact XML parser and object model.
 *
 * TinyXML is aimed mainly at configuration files and the like.
 *
 * The TinyXML object model is navigated using a path syntax of two forms.  If tx is
 * a TinyXML object, you can say:
 *
 * XMLElement e = tx("path_string");
 * XMLElement e = tx << "path_string"
 *
 * These both perform the same navigation operation, but while the first simply hands
 * you a reference to the target element, the second can do the same, but also sets the
 * tx object context.
 *
 */
class TinyXML
{
   protected static const char[510] _spaces = ' ';
   protected static const char[] _WS = "(\\s+)";
   protected static const char[] _TWS = "\\s+$";
   protected static const char[] _COMMENT = "(<!--(.*)-->)";
   protected static const char[] _XJOINT = r"(\.|(([A-Za-z_][0-9A-Za-z_]*)|\^|#|!|')(\[([0-9]+|\$)\])?)";
   protected static const char[] _AJOINT = r"(@(([A-Za-z_][0-9A-Za-z_]*)|(\[[0-9]+\])))";
   protected static RegExp _wsrex;
   protected static RegExp _twsrex;
   protected static RegExp _crex;
   protected static RegExp _jrex;

   static this()
   {
      //_spaces[0 .. 510] = ' ';
      _wsrex = RegExp(_WS);
      _twsrex = RegExp(_TWS);
      _crex = RegExp(_COMMENT);
      _jrex = RegExp(_XJOINT);
   }

   protected char[] prune(char[] s)
   {
      return _wsrex.find(s)? s: _wsrex.post;
   }

   protected char[] rprune(char[] s)
   {
      int pos = _twsrex.find(s);
      return (pos >= 0)? s[0 .. pos]: s;
   }

   /**
    * The XML element types supported by TinyXML.
    *
    * These are Tags - <tag> ... </tag>, text, as in <tag>This text</tag>, and comments
    * <!-- A comment -->
    */
   enum : int
   {
      ALL,
      TAG,
      TEXT,
      COMMENT
   }

   struct NVPair
   {
      char[] name;
      char[] value;
   }

   /**
    * The representation of a set of attributes on an XML tag.
    */
   class AttrList
   {
   private:
      NVPair[] list;
      int[char[]] map;
   public:

      /**
       * Add a new attribute to the list.
       *
       * Params:
       *   name = The attribute name.
       *   value = the attribute value.
       */
      void addAttr(char[] name, char[] value)
      {
         if (hasKey(name))
            throw new Exception("Attribute already exists");
         int n = list.length;
         list.length = n+1;
         list[n].name = name;
         list[n].value = value;
         map[name] = n;
      }

      /**
       * Create a new attribute list identical to the one for which the method was called.
       */
      AttrList clone()
      {
         AttrList a = new AttrList();
         int n = length();
         for (int i = 0; i < n; i++)
            a.addAttr(name(i), value(i));
         return a;
      }

      /**
       * Delete an attribute from the list by name
       *
       * Params:
       *   name = The name of the attribute to be deleted.
       */
      void delAttr(char[] name)
      {
         if (!hasKey(name))
            throw new Exception("No such attribute");
         int i = map[name];
         list = list[0 .. i] ~ list[i+1 .. $];
      }

      /**
       * A property-style method to get the length of the list
       */
      int length() { return list.length; }

      /**
       * Get the name of the i'th attribute in the list.
       *
       * Params:
       *   i = The desired index.
       */
      char[] name(int i) { return list[i].name; }
      
      /**
       * Get the name of the i'th value in the list.
       *
       * Params:
       *   i = The desired index.
       */
      char[] value(int i) { return list[i].value; }
      
      /**
       * Get the value of an attribute by name.
       *
       * Params:
       *   name = The desired name.
       */
      char[] value(char[] name) { return hasKey(name)? list[map[name]].value: null; }
      
      /**
       * Set the value of an attribute by name.
       *
       * Params:
       *   name = The name of the attribute.
       */
      char[] value(char[] name, char[] value)
         { if (hasKey(name)) { list[map[name]].value = value; return value; } else return null; }

      /**
       * Clear the list
       */
      void clear()
      {
         char[][] keys = map.keys;
         foreach (s; keys)
            map.remove(s);
         list.length = 0;
      }

      /**
       * Formats the list into a string suitable for XML.
       */
      char[] format()
      {
         char[] s = "";
         for (int i = 0; i < list.length; i++)
         {
            if (i > 0) s ~= " ";
            s ~= list[i].name ~ "=\"" ~ Tag.encode_entities(list[i].value) ~ "\"";
         }
         return s;
      }

   private:
      private bool hasKey(char[] key) { return ((key in map) != null); }
   }

   /**
    * A base class for Tags, Comments and body text - EText
    */
   class XMLElement
   {
      Tag _parent;

      /**
       * Abstract method to recover the element type.
       */
      abstract int EType();

      /**
       * Abstract method to format elements for XML.
       */
      abstract char[] format(char[] s, int indent);

      /**
       * Abstract method to recover a textual representation of the element.
       */
      abstract char[] PText();

      /**
       * Abstract method to recover an 'attribute' from the element
       */
      abstract char[] getValue(char[] name);

      /**
       * Abstract method to set an 'attribute' of the element.
       */
      abstract char[] setValue(char[] name, char[] value) ;

      /**
       * Abstract method to recover the parent of the element, which will
       * always be a Tag.
       */
      abstract Tag Parent();
   }

   /**
    * Class representing XML Tag body text.
    */
   class EText : XMLElement
   {
      char[] _etext;

      /**
       * Create an EText object and link it to a Tag.
       */
      this(char[] s, Tag parent) { _etext = s; _parent = parent; }

      /**
       * Get the element type - in this case TEXT.
       */
      int EType() { return TEXT; }

      /**
       * Format the text for inclusion in XML text.
       *
       * Params:
       *   s = An existing string to which the text representing this element is appended.
       *   indent = Degree to which the formatted text should be indented.
       *
       * Returns: The original string with the element text appended.
       */
      char[] format(char[] s, int indent)
      {
         char[] sp = _spaces[0 .. 3*indent];
         return (s ~ sp ~ _etext ~ "\n");
      }

      /**
       * Get a plain text version - just the text.
       */
      char[] PText() { return _etext; }

      /**
       * Get an 'attribute' by name - in this class the name is ignored.
       *
       * Params:
       *   name = A string value that is ignored.
       *
       * Returns: The plain text.
       */
      char[] getValue(char[] name) { return _etext; }

      /**
       * Set an 'attribute' by name - in this class the name is ignored.
       *
       * Params:
       *   name = A string value that is ignored.
       *   value = the value to be set for the text.
       *
       * Returns: The set plain text.
       */
      char[] setValue(char[] name, char[] value) { _etext = value; return _etext; }

      /**
       * Get the element's parent.
       */
      Tag Parent() { return _parent; }
   }

   /**
    * Class representing an XML comment.
    */
   class Comment : XMLElement
   {
      char[] _cmt;

      /**
       * Create a Comment object and link it to a Tag.
       */
      this(char[] s, Tag parent) { _cmt = s; _parent = parent; }

      /**
       * Get the element type - in this case COMMENT.
       */
      int EType() { return COMMENT; }

      /**
       * Format the text for inclusion in XML text.
       *
       * Params:
       *   s = An existing string to which the text representing this element is appended.
       *   indent = Degree to which the formatted text should be indented.
       *
       * Returns: The original string with the element text appended.
       */
      char[] format(char[] s, int indent)
      {
         char[] sp = _spaces[0 .. 3*indent];
         return (s ~ sp ~ "<!--" ~ _cmt ~ "-->\n");
      }

      /**
       * Get a plain text version - the bare comment.
       */
      char[] PText() { return "<!--" ~ _cmt ~ "-->"; }

      /**
       * Get an 'attribute' by name - in this class the name is ignored.
       *
       * Params:
       *   name = A string value that is ignored.
       *
       * Returns: The plain text.
       */
      char[] getValue(char[] name) { return _cmt; }

      /**
       * Set an 'attribute' by name - in this class the name is ignored.
       *
       * Params:
       *   name = A string value that is ignored.
       *   value = the value to be set for the text.
       *
       * Returns: The set plain text.
       */
      char[] setValue(char[] name, char[] value) { _cmt = value; return _cmt; }

      /**
       * Get the element's parent.
       */
      Tag Parent() { return _parent; }
   }


   /**
    * Class representing an XML Tag - &lt;tagname> ... &lt;/tagname>.
    *
    * Tags are the structural building blocks of the TinyXML object model.
    */
   class Tag : XMLElement
   {
      const char[] _TAG = "(<[A-Za-z_][A-Za-z0-9_]*[^>]*>)";
      const char[] _END = "(<\\/[A-Za-z_][A-Za-z0-9_]*\\s*>)";
      const char[] _AS = "(=[\"'])";

      static RegExp _xmlrex;
      static RegExp _tagrex;
      static RegExp _endrex;
      static RegExp _asrex;

      static this()
      {
         _xmlrex = RegExp(_XML);
         _tagrex = RegExp(_TAG);
         _endrex = RegExp(_END);
         _asrex = RegExp(_AS);
      }

      bool _valid;
      char[] _name;
      XMLElement[] _elist;
      AttrList _attributes;


      /**
       * Create a Tag object from XML text and possibly link it to a Tag.
       *
       * This constructor is called recursively to nibble Tag elements from a string
       * originally representing the entire XML, and is in essence the XML parser.
       *
       * Params:
       *   so = The source TinyXML object - provides the context and the text to be parsed.
       *        If so is null this constructor just returns a bare Tag element.
       *   parentnode = The tag that will be the parent of the new Tag.
       */
      this(TinyXML so, Tag parentnode)
      {
         _valid = false;
         _parent = parentnode;
         _attributes = new AttrList();
         if (so is null)
         {
            // Nothing to parse - just making a bare Tag object
            _valid = true;
            return;
         }

         so._src = prune(so._src);
         int pt = _tagrex.find(so._src);
         if (pt == -1)
         {
            so.setErr("No opening tag found");
            return;
         }
         char[] tag = _tagrex.match(1);
         int taglen = tag.length;
         int tagend = taglen-1;

         bool mt = (so._src[tagend-1] == '/');
         so._atTop = false;

         int ps = _wsrex.find(so._src);
         char[] as;
         if (ps == -1 || ps > tagend)
            _name = so._src[1 .. (mt? tagend-1: tagend)];
         else
         {
            char[] n = so._src[1 .. ps];
            _name = prune(n);
            as = prune(so._src[ps+1 .. (mt? tagend-1: tagend)]);
            as = rprune(as);
         }

         if (as.length) 
         {
            if ((!parse_attribs(so, as)))
            {
               return;
            }
         }
         if (so._atTop && mt)
         {
            _valid = true;
            return;
         }

         so._src = prune(so._src[tagend+1 .. so._src.length]);
         int n = 0;
         if (!mt)
         {
            // We have stripped of the opening tag construct and any whitespace, so now may have
            // Some element text
            // <!-- some comment -->
            // <A ...> ... </A>
            // Some more element text
            // <B ...> ... </B>
            // </CURRENTTAG  >

            for (;;)
            {
               if (so._src == "")
               {
                  setErr("Missing end tag after tag: " ~ _name);
                  return;
               }
               if (_endrex.find(so._src) == 0)
               {
                  // Hopefully we found the closing tag of the element we are parsing
                  char[] et = _endrex.match(1);
                  char[] tn = rprune(et[2 .. et.length-1]);    // gets us "CURRENTTAG  " --> "CURRENTTAG"
                  if (tn == _name)
                  {
                     so._src = prune(_endrex.post);
                     break;
                  }
                  so.setErr("Unexpected closing tag: " ~ et ~ " after " ~ _name);
                  return;
               }
               else if (_tagrex.find(so._src) == 0)
               {
                  Tag nn = new Tag(so, this);
                  if (!nn._valid) {
                     // error already reported
                     return;
                  }
                  _elist.length = _elist.length+1;
                  _elist[n] = nn;
                  n++;
               }
               else if (_crex.find(so._src) == 0)
               {
                  _elist.length = _elist.length+1;
                  _elist[n++] = new Comment(_crex.match(2), this);
                  so._src = prune(_crex.post);
               }
               else
               {
                  // It is element body text of some sort
                  int limit = indexOf(so._src, '<');
                  if (limit == -1)
                  {
                     so.setErr("Tag or closing tag expected after element text");
                     return;
                  }
                  char[] t = rprune(so._src[0 .. limit]);
                  _elist.length = _elist.length+1;
                  _elist[n++] = new EText(t, this);
                  so._src = so._src[limit .. $];   // no need to prune
               }
            }
         }
         this._valid = true;
      }

      /**
       * Get the Tag name - same as PText().
       */
      char[] Name() { return _name; }

      /**
       * Get the element type - in this case TAG.
       */
      int EType() { return TAG; }

      /**
       * Get a plain text version - in this case simply the Tag name.
       */
      char[] PText() { return _name; }

      /**
       * Get the element's parent.
       */
      Tag Parent() { return _parent; }

      private bool parse_attribs(TinyXML so, char[] as)
      {
         int pos = _asrex.find(as);
         if (pos == -1)
         {
            so.setErr("Bad attribute list - no name=\" construct found: " ~ as);
            return false;
         }
         char[] tail;
         for (int i = 0; pos != -1; i++) {
            char[] at = _asrex.match(1);
            tail = _asrex.post;
            char[] n = _asrex.pre;
            char quot = at[1];
            int q2 = std.string.find(tail, quot);
            if (q2 == -1)
            {
               so.setErr("Bad attribute list - missing closing quote: " ~ as);
               return false;
            }
            if (_wsrex.find(n) != -1)
            {
               so.setErr("Bad attribute list - space in attribute name: " ~ as);
               return false;
            }
            char[] t = tail[0 .. q2];
            char[] v = decode_entities(t);
            _attributes.addAttr(n, v);
            tail = tail[q2+1 .. tail.length];
            as = prune(tail);
            pos = _asrex.find(as);
         }
         if (tail.length) {
            pos = _wsrex.find(tail);
            if (pos != -1)
            {
               so.setErr("Bad attribute list - garbage after attributes: " ~ as);
               return false;
            }
            if (tail.length != _wsrex.match(1).length)
            {
               so.setErr("Bad attribute list - garbage after attributes: " ~ as);
               return false;
            }
         }

         return true;
      }

      /**
       * Format the text for inclusion in XML text.
       *
       * Params:
       *   s = An existing string to which the text representing this element is appended.
       *   indent = Degree to which the formatted text should be indented.
       *
       * Returns: The original string with the element text appended.
       */
      char[] format(char[] s, int indent)
      {
         char[] sp = _spaces[0 .. 3*indent];
         s ~= sp ~ "<" ~ _name;
         if (_attributes.length())
         {
               s ~= " " ~ _attributes.format();
         }
         bool mt = (_elist.length == 0);
         if (mt)
            s ~= "/>\n";
         else
         {
            s ~= ">";
            if (_elist.length == 1 && _elist[0].EType() != TAG)
            {
               s ~= _elist[0].PText() ~ "</" ~ _name ~ ">\n";
            }
            else
            {
               s ~= "\n";
               for (int i = 0; i < _elist.length; i++)
               {
                  s = _elist[i].format(s, indent+1);
               }
               s ~= sp ~ "</" ~ _name ~ ">\n";
            }
         }
         return s;
      }

      /**
       * Decode &amp;lt; and &amp;amp; to < and &.
       *
       * Params:
       *   s = The string to be decoded.
       *
       * Returns: The decoded string.
       */
      public static char[] decode_entities(char[] s)
      {
         s = std.string.replace(s, "&lt;", "<");
         return std.string.replace(s, "&amp;", "&");
      }

      /**
       * Encode < and & to &amp;lt; and &amp;amp;.
       *
       * Params:
       *   s = The string to be encoded.
       *
       * Returns: The encoded string.
       */
      public static char[] encode_entities(char[] s)
      {
         s = std.string.replace(s, "&", "&amp;");
         return std.string.replace(s, "<", "&lt;");
      }

      /**
       * Get an 'attribute' by name - in this class the name is used.
       *
       * Params:
       *   name = The name of the attribute.
       *
       * Returns: The attribute value.
       */
      char[] getValue(char[] name)
      {
         char[] rv = _attributes.value(name);
         if (rv is null)
            throw new Exception("No such attribute: " ~ name);
         return rv;
      }

      /**
       * Set an 'attribute' by name - in this class the name is used.
       *
       * Params:
       *   name = The name of the attribute.
       *   value = the value to be set for the attribute.
       *
       * Returns: The set attribute value.
       */
      char[] setValue(char[] name, char[] value)
      {
         char[] rv = _attributes.value(name, value);
         if (rv is null)
            throw new Exception("No such attribute: " ~ name);
         return rv;
      }

      /**
       * Add a new attribute to the Tag element.
       *
       * Params:
       *   name = The attribute name.
       *   value = the attribute value.
       */
      void addAttribute(char[] name, char[] value)
      {
         _attributes.addAttr(name, value);
      }

      /**
       * Delete an attribute from the Tag element by name
       *
       * Params:
       *   name = The name of the attribute to be deleted.
       */
      void deleteAttribute(char[] name)
      {
         _attributes.delAttr(name);
      }

      /**
       * Append an element to the child list of this Tag.
       *
       * Params:
       *   e = and XMLElement object.
       */
      void appendElement(XMLElement e)
      {
         _elist = _elist ~ e;
      }

      /**
       * Duplicate this Tag object, attaching the duplicate immediately after
       * this object in the parent object's child list.
       *
       * Returns: The duplicated object.
       */
      Tag duplicate()
      {
         Tag parent = _parent;
         int i;
         for (i = 0; i < parent._elist.length; i++)
         {
            if (cast(Object) (parent._elist[i]) == cast(Object) this)
               break;
         }
         parent._elist = parent._elist[0 .. i+1] ~ cast(XMLElement) this ~ parent._elist [i+1 .. $]; 
         return cast(Tag) parent._elist[i+1];
      }

      /**
       * Get an array of text items in the body of this Tag.
       *
       * These are fragments that are separated by child Tags or comments.*
       * Returns: An array of strings.
       */
      char[][] getTextFragments()
      {
         char[][] rv;
         rv.length = _elist.length;
         int n = 0;
         for (int i = 0; i < _elist.length; i++)
         {
            if (_elist[i].EType() == TEXT)
            {
               rv[i] = _elist[i].PText();
               n++;
            }
         }
         rv.length = n;
         return rv;
      }


      /**
       * Get an array of comment text items in the body of this Tag.
       *
       * Returns: An array of strings.
       */
      char[][] getComments()
      {
         char[][] rv;
         rv.length = _elist.length;
         int n = 0;
         for (int i = 0; i < _elist.length; i++)
         {
            if (_elist[i].EType() == COMMENT)
            {
               rv[i] = _elist[i].PText();
               n++;
            }
         }
         rv.length = n;
         return rv;
      }

      private XMLElement addAdjacent(XMLElement e, int rel)
      {
         if (_parent is null)
            throw new Exception("Can't add an element after this tag - no parent");
         int i;
         for (i = 0; i < _parent._elist.length; i++)
         {
            if (cast(Object) (_parent._elist[i]) == cast(Object) this)
               break;
         }
         e._parent = _parent;
         _parent._elist = _parent._elist[0 .. i+rel] ~ e ~ _parent._elist [i+rel .. $]; 
         return e;
      }

      /**
       * Add an XMLElement after this Tag in its parent's child list.
       *
       * Params:
       *   e = An XMLElement object to be added.
       *
       * Returns: The added element.
       */
      XMLElement addAfter(XMLElement e) { return addAdjacent(e, 1); }

      /**
       * Add an XMLElement before this Tag in its parent's child list.
       *
       * Params:
       *   e = An XMLElement object to be added.
       *
       * Returns: The added element.
       */
      XMLElement addBefore(XMLElement e) { return addAdjacent(e, 0); }

      private int countElements(int type)
      {
         int n = 0;
         foreach (x; _elist)
         {
            if (x.EType() == type)
               n++;
         }
         return n;
      }

      /**
       * Get the number of Tag objects in the child list of this Tag.
       *
       * Returns: Number of Tags.
       */
      int Tags() { return countElements(TAG); }

      /**
       * Get the number of EText objects in the child list of this Tag.
       *
       * Returns: Number of EText objects.
       */
      int ETexts() { return countElements(TEXT); }

      /**
       * Get the number of Comment objects in the child list of this Tag.
       *
       * Returns: Number of Comment objects.
       */
      int Comments() { return countElements(COMMENT); }

      /**
       * Get the number of elements in the child list of this Tag.
       *
       * Returns: Total number of child elements.
       */
      int Elements() { return _elist.length; }

      /**
       * Returns a child element of a specified type at a specified index.
       * 
       * The index is relative to the number of child elements of that type.
       *
       * Returns: The child element.
       *
       * To use the returned object, you must cast it to the type you specified, as in
       * Tag t = cast(Tag) ptag[1, TAG];
       */
      XMLElement opIndex(int index, int type)
      {
         if (type == 0)
            return _elist[index];
         if (index < 0 || index >= _elist.length)
            return null;
         int found = 0;
         for (int i = 0; i < _elist.length; i++)
         {
            if (_elist[i].EType() == type)
            {
               found++;
               if (found == index+1)
                  return _elist[i];
            }
         }
         return null;
      }

      /**
       * Returns the first EText child element of this Tag.
       *
       * This is intended for use in the common case where the tag is like
       * &lt;tag>Body text&lt;/tag>
       *
       * Returns: The child element.
       */
      char[] FirstText()
      {
         if (ETexts() < 1)
            throw new Exception("Tag has no text elements");
         return opIndex(0, TEXT).PText();
      }
   }

   const char[] _XML = "(<\\?xml +version *= *\"1.\\d\"( +encoding *= *\"[A-Za-z0-9\\-_]*\")? *\\?>\\s*)";

   protected char[] _src;
   char[] _xmlDecl;
   char[][] _outerComments;
   bool _atTop;
   Tag _top;
   RegExp _xmlrex;
   int findex;
   char[] _err;
   Tag _context;

   protected void setErr(char[] msg)
   {
      _err = msg;
   }

   private char[] choppreamble(char[] s)
   {
      int i = _xmlrex.find(s);
      if (i == -1)
         return null;
      _xmlDecl = rprune(_xmlrex.match(1));
      return _xmlrex.post;
   }

   private char[] getPreComments(char[] s)
   {
      int n = 0;
      while (_crex.find(s) == 0)
      {
         _outerComments.length = _outerComments.length+1;
         _outerComments[n++] = _crex.match(1);
         s = prune(_crex.post);
      }
      return s;
   }

   private XMLElement findElement(char[] txpath, bool setcontext)
   {
      Tag te;
      int tindex;
      char[] tname;
      bool hasattrib = false;
      if (txpath[0] == '/')
         txpath = txpath[1 .. $];
      char[][] a = std.string.split(txpath, "/");
      for (int i = 0; i < a.length; i++)
      {
         if (_jrex.find(a[i]) != 0 || _jrex.post.length > 0)
            throw new Exception("Bad joint at " ~ a[i]);
         if (a[i][0] == '^')
         {
            if (i > 0)
               throw new Exception("Only the first joint can begin with '^'");
            a[i] = _top._name ~ a[i][1 .. $];
         }
         if (a[i][0] == '.')
         {
            if (i > 0)
               throw new Exception("Only the first joint can begin with '.'");
         }
         if (a[i][0] == '!' || a[i][0] == '\'')
         {
            if (i < a.length-1)
               throw new Exception("No further nodes allowed after Text or Comment joints");
         }
      }
      if (a[0][0] != '.' && indexOf(a[0], _top._name) != 0)
         throw new Exception("Top joint does not match XML outer tag: " ~ a[0] ~ _top._name);
      if (indexOf(a[0], '[') >= 0)
         throw new Exception("Top joint can't be indexed: " ~ a[0]);

      void extractIndex(char[] s)
      {
         int ob = indexOf(s, '[');
         if (ob == -1)
         {
            tname = s;
            tindex = 0;
            return;
         }
         tname = s[0 .. ob];
         int cb = indexOf(s, ']');
         char[] inner = s[ob+1 .. cb];
         if (inner == "$")
            tindex = -1;
         else
            tindex = cast(int) atoi(inner);
      }
      if (a[0][0] != '.')
      {
         te = _top;  // Otherwise navigate from existing context
         _context = _top;
      }
      else
         te = _context;
      int index = 0;
   mainloop:
      for (int i = 1; i < a.length; i++)
      {
         extractIndex(a[i]);
         int found = 0;
         if (indexOf("#!'", a[i][0]) >= 0)
         {
            // Search purely by position
            int EType = (a[i][0] == '#')? TAG:((a[i][0] == '!')? COMMENT: TEXT);
            if (tindex >= 0)
            {
               for (int j = 0; j < te._elist.length; j++)
               {
                  if (te._elist[j].EType() == EType)
                  {
                     found++;
                     if (found > tindex)
                     {
                        if (i == a.length-1)
                        {
                           if (setcontext)
                           {
                              if (EType == TAG)
                                 _context = cast(Tag) te._elist[j];
                              else
                                 _context = cast(Tag) te;
                           }
                           return te._elist[j];
                        }
                        te = cast(Tag) te._elist[j];
                        continue mainloop;
                     }
                  }
               }
               throw new Exception("Joint index out of range: " ~ a[i]);
            }
            else
            {
               for (int j = te._elist.length-1; j >= 0; j--)
               {
                  if (te._elist[j].EType() == EType)
                  {
                     if (i == a.length-1)
                     {
                        if (setcontext)
                        {
                           if (EType == TAG)
                              _context = cast(Tag) te._elist[j];
                           else
                              _context = cast(Tag) te;
                        }
                        return te._elist[j];
                     }
                     te = cast(Tag) te._elist[j];
                     continue mainloop;
                  }
               }
               throw new Exception("Joint index out of range: " ~ a[i]);
            }
         }
         else
         {
            // Search for a tag by name and index
            if (tindex >= 0)
            {
               for (int j = 0; j < te._elist.length; j++)
               {
                  if (te._elist[j].EType() == TAG && te._elist[j].PText() == tname)
                  {
                     found++;
                     if (found > tindex)
                     {
		        
                        if (i == a.length-1)
                        {
                           if (setcontext)
                           {
                              _context = cast(Tag) te._elist[j];
		                     }
                           return te._elist[j];
                        }
                        te = cast(Tag) te._elist[j];
                        continue mainloop;
                     }
                  }
               }
               throw new Exception("Joint not found: " ~ a[i]);
            }
            else
            {
               for (int j = te._elist.length-1; j >= 0; j--)
               {
                  if (te._elist[j].EType() == TAG && (cast(Tag) te._elist[j])._name == tname)
                  {
                     if (i == a.length-1)
                     {
                        if (setcontext)
                        {
                           _context = cast(Tag) te._elist[j];
                        }
                        return te._elist[j];
                     }
                     te = te = cast(Tag) te._elist[j];
                     continue mainloop;
                  }
               }
               throw new Exception("Joint not found: " ~ a[i]);
            }
         }
      }
      return te;
   }

/**
 * The public methods of TinyXML
 */
public:
   /**
    * Construct a TinyXML object from a string.
    */
   this(char[] s)
   {
      _xmlrex = RegExp(_XML);
      _src = choppreamble(s);
      _src = getPreComments(_src);
      _atTop = true;
	   _top = new Tag(this, null);
      _context = _top;
   }

   /**
    * Construct a TinyXML object from a File.
    */
   this(File f)
   {
      this(f.toString());
   }

   /**
    * Generate formated XML text from a TinyXML object.
    */
   public char[] XML()
   {
      char[] s = _xmlDecl ~ "\n";
      foreach (char[] c; _outerComments)
         s ~= c ~ "\n";
      return s ~ _top.format("", 0);
   }

   /**
    * Navigate to (access) an element in the TinyXML object model, setting the context to the found object
    * if it is a Tag.
    *
    * Use as in: XMLElement x = txml << "path_expression";
    *
    * Params:
    *   xpath = A string describing the required destination element.<br>
    * Usage:
    * The xpath parameter may take the following forms:<br>
    * /outer_tagname/tagname1/tagname2 ... - find a Tag element by element names<br>
    * ^/tagname1/tagname2 ...    - equivalent to the above
    * 
    * /outer_tagname/tagname1/#[0] - find a Tag by its offset under its parent<br>
    * ^/tagname1/#[0] - equivalent
    *
    * /outer_tagname/tagname1/![0] - find a Comment by its offset under its parent<br>
    * ^/tagname1/![0] - equivalent
    *
    * /outer_tagname/tagname1/'[0] - find a text element by its offset under its parent<br>
    * ^/tagname1/'[0] - equivalent
    *
    * ./... - find an element down a path relative to the current context
    */
   XMLElement opShl(char[] xpath) { return findElement(xpath, true); }

   /**
    * Navigate to (access) an element in the TinyXML object model, without setting the context.
    *
    * Use as in: XMLElement x = txml["path_expression"];
    *
    * Params:
    *   xpath = A string describing the required destination element.<br>
    *
    * As described for opShl
    */
   XMLElement opIndex(char[] xpath) { return findElement(xpath, false); }

   /**
    * Return the Tag that is the current context.
    */
   Tag Context() { return _context; }

   /**
    * Create a Tag element.
    *
    * Params:
    *   name = Name of the Tag.
    * Returns: The new Tag.
    */
   Tag makeTag(char[] name)
   {
      Tag t = new Tag(null, _context);
      t._name = name;
      return t;
   }

   /**
    * Create a Comment element.
    *
    * Params:
    *   text = Text of the Comment.
    * Returns: The new Comment.
    */
   Comment makeComment(char[] text)
   {
      Comment c = new Comment(text, _context);
      return c;
   }

   /**
    * Create an EText element.
    *
    * Params:
    *   text = Text of the element.
    * Returns: The new EText object.
    */
   EText makeEText(char[] text)
   {
      EText t = new EText(text, _context);
      return t;
   }

   /**
    * Return the name of the top-level Tag.
    */
   char[] OuterName()
   {
	   return _top._name;
   }

   /**
    * Check if test passed to the constructor parsed correctly.
    */
   bool ok()
   {
      return _top._valid;
   }

   /**
    * Get the error message if it did not parse correctly.
    */
   char[] error() { return _err; }
}

alias TinyXML.AttrList AttrList;
alias TinyXML.EText EText;
alias TinyXML.Comment Comment;
alias TinyXML.Tag Tag;
alias TinyXML.XMLElement XMLElement;

/+
// Here is an example of usage.
char[] src = 
"<?xml  version = \"1.0\" encoding=\"utf8\" ?>\n" ~
"<!-- Pre comment 1 -->\n" ~
"<!-- Pre comment 2 -->\n" ~
"<individual name=\"John Doe\" age=\"&lt; 30 &amp; handsome\">\n" ~
"      <!-- A comment -->\n" ~
"      Some element text." ~
"      <street a=\"a\" b=\"b\">22 Washington Ave.        <house></house></street>\n" ~
//"      <street>22 Washington Ave.        <house></house>\n" ~
"      <city><!-- city comment --></city>\n" ~
"      <!-- Another comment -->\n" ~
"      <state>                  NJ                            </state >\n" ~
"      <zip><whatever /></zip>\n" ~
"      Some more element text." ~
"      <list length=\"3\">\n" ~
"         <item>A</item>\n" ~
"         <item>B<!--xxx--></item>\n" ~
"         <item>C</item>\n" ~
"      </list>\n" ~
"      Yet more element text." ~
"</individual >";

void main(char[][] args)
{
   TinyXML tx = new TinyXML("<?xml  version = \"1.0\"?><foo bar=\"bar\" />");
   writefln(tx.XML());
   tx = new TinyXML(src);
   if (!tx.ok)
   {
      writefln(tx.error);
      return;
   }
   writefln(tx.XML);

   writefln("1)");
   writefln(tx.OuterName);

   writefln("2)");
   XMLElement e = tx << "/individual/city/!";
   writefln(e.PText);
   writefln(tx.Context().Name);
   e = tx["^/list/#[1]/!"];
   writefln(e.PText);

   writefln("3)");
   e = tx << "^/list";
   writefln(e.PText());
   writefln(tx.Context().PText);
   e = tx["./#[1]/!"];
   writefln(e.PText);

   writefln("4)");
   e = tx << "^/#";
   writefln(e.PText);
   tx.Context().addBefore(tx.makeComment("Comment added"));
   writefln(tx.XML);

   writefln("5)");
   e = tx["^/#[$]"];
   writefln(e.PText);

   writefln("6)");
   e = tx["^/list/#[1]/![$]"];
   writefln(e.PText);

   writefln("7)");
   e = tx << "^";
   writefln(e.PText);
   int n = tx.Context.Elements;
   for (int i=0; i < n; i++)
   {
      writefln(tx.Context[i, TinyXML.ALL].PText);
   }
   writefln("8)");
   n = tx.Context.Tags;
   for (int i=0; i < n; i++)
   {
      XMLElement x = tx["./#[" ~ toString(i) ~ "]"];
      writefln(x.PText);
   }

   writefln("9)");
   Tag top = tx.Context;
   for (int i=0; i < n; i++)
   {
      Tag y = cast(Tag) top[i, TinyXML.TAG];
      writefln(y.PText);
   }
   writefln(top.FirstText);

   writefln("10)");
   e = tx["^/street"];
   writefln(e.getValue("a"));
}
+/
