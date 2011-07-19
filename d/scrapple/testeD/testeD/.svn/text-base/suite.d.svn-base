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

module suite;
/*
 * Suite
 * Creates an environment to collect testresults. The instances of
 * this class are globally accessible through a factory.
 *
 * $Author$
 * $Revision: 15 $
 * $Date: 2007-10-01 22:06:44 +0200 (Mon, 01 Oct 2007) $
 */
import unit;

typedef void delegate(unit_result) unit_observer_t;
typedef void delegate(suite_result) suite_observer_t;

static suite Suite(char[] name)
  in
    {
      assert(name.length > 0);
    }
out(suite)
     {
	 assert(suite !is null);
     }
body
  {
    suite Suite = null;
    if(name in suite.suite_list)
      Suite = suite.suite_list[name];
    else
      {
	Suite = suite.suite_list[name] = new suite(name);
      }
    return Suite;
  }

void InformAbout(unit_observer_t observer)
  in{assert(observer !is null);}
body
  {
    foreach(Suite; suite.suite_list)
      {
	Suite.inform(observer);
      }
  }

void InformAbout(suite_observer_t observer)
  in{assert(observer !is null);}
body
  {
    foreach(Suite; suite.suite_list)
      {
	Suite.inform(observer);
      }
  }
    
    
class suite
{
  /*
   * Suite creation
   * Instanciation of suite is only possible using the factory method
   * createSuite. It can be used at an arbitrary number of places to 
   * test code blocks.
   */

package:
  char[] name;
  static suite[char[]] suite_list;
  this(char[] name)
  {
    this.name = name;
  }
public:

  /*
   * Suite observation
   * The suite dispatches its results through delegates to an arbitrary number
   * of observers.
   */
private:
  unit_observer_t[] unit_observers;
  suite_observer_t[] suite_observers;

public:
  void emit(unit_result result)
  {    
    foreach(observer; unit_observers)
      observer(result);
  }
  void emit(suite_result result)
  {
    foreach(observer; suite_observers)
      observer(result);
  }
  void inform(unit_observer_t newObserver)
    in
      {
	assert(newObserver !is null);
      }
  body
  {  
    unit_observers.length = unit_observers.length + 1;
    unit_observers[unit_observers.length - 1] = newObserver;
  }
  void inform(suite_observer_t newObserver)
    in
      {
	assert(newObserver !is null);
      }
  body
  {
    suite_observers.length = suite_observers.length + 1;
    suite_observers[suite_observers.length - 1] = newObserver;
  }

  /*
   * Suite test management
   * The suite maintains a list of tests to execute. They are excecuted either on demand (using 
   * the method 'execute()' or on destruction.
   */

private:
  unit[] test_list;
  unit_result[] result_list;
  int test_count;
  int success_count;

public:
  void addTest(unit newTest)
  {
    test_list.length = test_list.length + 1;
    test_list[test_list.length - 1] = newTest;
    //execute(newTest);
  }
  void execute(unit test)
  {
    unit_result result = test.execute();
    if(result.success)
      success_count++;
    emit(result);
    store(result);
  }
  void executeAll()
  {
    foreach(test; test_list)
      {
	execute(test);
      }
  }
  void store(unit_result result)
  {
    result_list.length = result_list.length + 1;
    result_list[$-1] = result;
  }
  static ~this()
  {
    foreach(suite; suite_list)
      {
	suite.executeAll();
	suite.emit(new suite_result(suite.name, suite.result_list.length, suite.success_count));
      }
  }
}

class suite_result : result
{
 private:
  int unit_count;
  int success_count;
  char[] test_name;

 public:
  this(char[] test_name, int unit_count, int success_count)
    in
      {
	assert(unit_count >= 0);
	assert(success_count >= 0);
      }
  body
    {
      super(success_count == unit_count);
      this.test_name = test_name;
      this.unit_count = unit_count;
      this.success_count = success_count;
    }
  
  unittest
    {
      Suite("A suite").addTest(unit.unit.test("In-class test", "fail", {return false;}));
    }
  char[] toString()
  {
    char[] buf = "";
    buf ~= test_name ~ ":";
    buf ~= std.string.toString(unit_count) ~ "/" ~ std.string.toString(success_count);
    buf ~= "(" ~ std.string.toString(cast(float)(success_count)/unit_count * 100) ~ "%)";
    return buf;
  }
}


private import std.stdio;
unittest
{
  Suite("A suite").addTest(unit.unit.test("Test from suite.d", "failmessage", {return false;}));
}

unittest
{
  void observe_unit(unit_result result)
  {
    writefln("%s", result.toString());
  }
  Suite("A suite").inform(&observe_unit);
  void observe_suite(suite_result result)
  {
    writefln("%s", result.toString());
  }
  Suite("A suite").inform(&observe_suite);
}