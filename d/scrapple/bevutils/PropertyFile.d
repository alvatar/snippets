/**
 * Authors: Steve Teale - steve.teale@britseyeview.com
 *
 * Date: 2007/05/19
 * History: V0.1
 * License: Use freely for any purpose.
 */
module bevutils.propertyfile;

import std.string;
import std.stdio;
import std.stream;
import std.regexp;
import bevutils.tinyxml;

/**
 * A class to facilitate reading of XML property files.
 *
 * Currently supports string, int, string[] and int[] properties.
 * 
 * Two styles of XML are supported:
 * ------------------------------------------------------
<?xml version="1.0" ?>
   <Properties layout="1">
      <constr type="string">Driver=SQL Server;DSN=MySQL1</constr>
      <logpath type="string">d:\logs</logpath>
      <numlogs type="int">10</numlogs>
      <maxlogsize type="int">1000000</maxlogsize>
      <watchdirs type="string[]">
         <item>d:\A</item>
         <item>d:\B</item>
         <item>d:\C</item>
      </watchdirs>
      <thingie type="int[]">
         <item>1</item>
         <item>2</item>
         <item>3</item>
      </thingie>
   </Properties>
 * ------------------------------------------------------
 * and:
 * ------------------------------------------------------
<?xml version="1.0" ?>
<Properties layout="2">
   <constr type="string" value="Driver=SQL Server;DSN=MySQL1" />
   <logpath type="string" value="d:\logs" />
   <numlogs type="int" value="10" />
   <maxlogsize type="int" value="1000000" />
   <watchdirs type="string[]">
      <item value="d:\A" />
      <item value="d:\B" />
      <item value="d:\C" />
   </watchdirs>
   <thingie type="int[]">
      <item value="1" />
      <item value="2" />
      <item value="3" />
   </thingie>
</Properties>
 * ------------------------------------------------------
 */
class PropertyFile
{
private:
   struct Property
   {
      int type;
      union
      {
         char[] s;
         int i;
         char[][] as;
         int[] ai;
      }
   }

   Property [char[]] _props;
   static RegExp irex;

public:
   static this()
   {
      irex = RegExp("-?[0-9]{1,10}");
   }

   /**
    * Constructor from a file name
    *
    * Params:
    *   filepath = Fully qualified file name.
    */
   this(char[] filepath)
   {
      File file = new File(filepath);
      char[] text = file.toString();
      TinyXML tx = new TinyXML(text);
      Tag t = tx.Context;
      char[] layout = t.getValue("layout");
      int lt = (layout == "1")? 1: 2;
      int n = t.Tags;
      for (int i = 0; i < n; i++)
      {
         Tag x = cast(Tag) t[i, TinyXML.TAG];
         char[] type = x.getValue("type");
         char[] name = x.Name;
         char[] val;

         Property p;
         switch (type)
         {
            case "int":
               val = (lt == 1)? x.FirstText: x.getValue("value");
               int m = irex.find(val);
               if (m != 0 || irex.post.length > 0)
                  throw new Exception(name ~ " Value " ~ val ~ " does not match the specified type.");
               p.i = cast(int) std.string.atoi(val);
               p.type = 1;
               _props[name] = p;
               break;
            case "int[]":
               handleIntArray(x, name, lt);
               break;
            case "string[]":
               handleStringArray(x, name, lt);
               break;
            default:
               val = (lt == 1)? x.FirstText: x.getValue("value");
               p.s = val;
               p.type = 0;
               _props[name] = p;
               break;
         }
      }
   }

   /**
    * Determine if a property is present by name.
    *
    * Params:
    *   s = Popery name.
    */
   bool opCall(char[] s) { return !((s in _props) is null); }

   /**
    * Get an integer property value.
    *
    * Params:
    *   name = Property name.
    */
   int getInt(char[] name)
   {
     if (!(name in _props))
        throw new Exception("No such property");
     Property p = _props[name];
     if (p.type != 1)
        throw new Exception("Property " ~ name ~ " is not of type int");
     return p.i;
   }

   /**
    * Get a string property value.
    *
    * Params:
    *   name = Property name.
    */
   char[] getString(char[] name)
   {
     if (!(name in _props))
        throw new Exception("No such property");
     Property p = _props[name];
     if (p.type != 0)
        throw new Exception("Property " ~ name ~ " is not of type string");
     return p.s;
   }

   /**
    * Get an int array property value.
    *
    * Params:
    *   name = Property name.
    */
   int[] getIntArray(char[] name)
   {
     if (!(name in _props))
        throw new Exception("No such property");
     Property p = _props[name];
     if (p.type != 3)
        throw new Exception("Property " ~ name ~ " is not of type int[]");
     return p.ai;
   }

   /**
    * Get an string array property value.
    *
    * Params:
    *   name = Property name.
    */
   char[][] getStringArray(char[] name)
   {
     if (!(name in _props))
        throw new Exception("No such property");
     Property p = _props[name];
     if (p.type != 2)
        throw new Exception("Property " ~ name ~ " is not of type string[]");
     return p.as;
   }

private:
   private void handleIntArray(Tag x, char[] name, int lt)
   {
      Property p;
      int tc = x.Tags;
      int[] a;
      a.length = tc;
      for (int i = 0; i < tc; i++)
      {
         Tag t = cast(Tag) x[i, TinyXML.TAG];
         char[] s = (lt == 1)? t.FirstText: t.getValue("value");
         int n = irex.find(s);
         if (n != 0 || irex.post.length > 0)
            throw new Exception(name ~ " array element " ~ s ~ " does not match the specified type.");
         n = cast(int) std.string.atoi(s);
         a[i] = n;
      }
      p.ai = a;
      p.type = 3;
      _props[name] = p;
   }

   private void handleStringArray(Tag x, char[] name, int lt)
   {
      Property p;
      int tc = x.Tags;
      char[][] a;
      a.length = tc;
      for (int i = 0; i < tc; i++)
      {
         Tag t = cast(Tag) x[i, TinyXML.TAG];
         a[i] = (lt == 1)? t.FirstText: t.getValue("value");
      }
      p.as = a;
      p.type = 2;
      _props[name] = p;
   }
}

/+
void main(char[][] args)
{
   PropertyFile pf = new PropertyFile("d:\\d\\test2.xml");
   writefln(pf.getString("constr"));
   writefln(pf.getString("logpath"));
   int n = pf.getInt("numlogs");
   writefln("%d", n);
   n = pf.getInt("maxlogsize");
   writefln("%d", n);
   char[][] as = pf.getStringArray("watchdirs");
   for (int i = 0; i < as.length; i++)
      writefln(as[i]);
   int[] ai = pf.getIntArray("thingie");
   for (int i = 0; i < ai.length; i++)
      writefln("%d", ai[i]);
}
+/
