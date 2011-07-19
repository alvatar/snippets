/*
 *   testeD - unit testing library for D
 *   Copyright (C) 2007 Lukas Laedrach
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

module unit;
/*
 * Unit
 * Encapsulates a single unittest.
 *
 * $Author$
 * $Date: 2007-10-01 22:06:44 +0200 (Mon, 01 Oct 2007) $
 */


private import std.date;
private import std.string;

class unit
{
protected:
  char[] name;
  char[] failmessage;
  bool delegate() testfunction;
  
  this(char[] name, char[] failmessage, bool delegate() test)
  {
    this.name = name;
    this.failmessage = failmessage;
    this.testfunction = test;
  }
public:

  static unit test(char[] name, char[] failmessage, bool delegate() test)
  {	
    return new unit(name, failmessage, test);
  }
  
  unit_result execute()
  {
    try
      {
	return new unit_result(testfunction(), name, failmessage);
      }
    catch
      {
	return new unit_result(false, name, failmessage);
      }
  }
};

class unit_except(E) : unit
{
 protected:
  this(char[] name, char[] failmessage, bool delegate() test)
    {
      super(name, failmessage, test);
    }
 public:
  static unit test(char[] name, char[] failmessage, bool delegate() test)
  {
    return new unit_except(name, failmessage, test);
  }
  unit_result execute()
  {
    try
      {
	return new unit_result(testfunction(), name, failmessage);
      }
    catch (E)
      {
	return new unit_result(true, name, failmessage);
      }
    catch
      {
	return new unit_result(false, name, failmessage);
      }
  }
}


class result
{
protected:
  bool p_success;
  d_time timestamp;

public:
  this(bool success)
  {
    this.p_success = success;
    timestamp = std.date.getUTCtime();
  }
  // Property
  bool success()
  {
    return p_success;
  }
  abstract char[] toString();
};

class unit_result : result
{
 private:
  char[] testname;
  char[] message;
 public:
  this(bool success, char[] testname, char[] message)
    {
      super(success);
      this.testname = testname;
      this.message = message;
    }

  char[] toString()
  {
    char[] ret;
    ret = std.date.toString(timestamp) ~" | " ~ testname ~ ": ";
    if(success)
      ret ~= "ok";
    else
      ret ~= "fail -- " ~ message;
		
    return ret;
  }
}


private import std.stdio;
unittest
{
  unit dut;
  unit_result result; 

  bool endsIn(char[] who, char[] what)
  {
    return who[who.length-what.length..who.length] == what;
  }
  assert(
	 endsIn( // Test on success with delegate test function
		unit.test("Test1", "Fail", {return true;}).execute().toString(), 
		"| Test1: ok"
		)
	 );

  assert(
	 endsIn( // Test on failure with delgate test function
		unit.test("Test2", "Fail", {return false;}).execute().toString(), 
		"| Test2: fail -- Fail"
		)
	 );
  assert(
	 endsIn( // Test on failure with exception
		unit_except!(unit_result).test("Test3", "Fail", {throw new unit_result(false, "", ""); return true;}).execute().toString(),
		"| Test3: ok"
		 )
	 );
}