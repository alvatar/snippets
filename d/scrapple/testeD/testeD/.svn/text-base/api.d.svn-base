/*
  testeD -- unit test library for D 
  Copyright (C) 2007 Lukas Laedrach
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

module testeD.api;
/*
 * Defines a set of functions to deal with unit testing
 */


private import suite;
import unit;

/*
 * Sets the default suite for the current file to NewDefaultSuite
 */
void DefaultSuite(suite NewDefaultSuite)
  in{assert(NewDefaultSuite !is null);}
body
{
  ModuleSuite[__FILE__] = NewDefaultSuite;
}
void DefaultSuite(char[] DefaultSuiteName)
  in{assert(DefaultSuiteName.length > 0);}
body
{
  DefaultSuite(suite.Suite(DefaultSuiteName));
}

/*
 * Set the default listener
 */
void DefaultUnitObserver(suite.unit_observer_t observer)
{
  getDefaultSuite().inform(observer);
}
void DefaultSuiteObserver(suite.suite_observer_t observer)
{
  getDefaultSuite().inform(observer);
}

/*
 * getDefaultSuite returns the suite for current file. If it does not
 * exist, it's created*/
suite getDefaultSuite()
  out(s){assert(s !is null);}
body
{
  if(__FILE__ in ModuleSuite)
    return ModuleSuite[__FILE__];
  return ModuleSuite[__FILE__] = suite.Suite(__FILE__);
}

/*
 * ModuleSuite holds for each module(file), which contains an
 * anonymous call for test, the suite
 */
static suite.suite[char[]] ModuleSuite;


/*
 * test Creates a new unit and adds it to the suite getDefaultSuite
 * returns. This means it's added either to a automatically generated
 * suite or a custom one (which was set with DefaultSuite before)
 */
void test(char[] name, char[] failmessage, bool delegate() test)
{
  getDefaultSuite().addTest(unit.unit.test(name, failmessage, test));
}

void test_except(E)(char[] name, char[] failmessage, bool delegate() test)
{
  getDefaultSuite().addTest(unit.unit_except!(E).test(name, failmessage, test));
}


private import std.stdio;
unittest
{
  // Simple tests
  assert(getDefaultSuite() !is null);
  assert(getDefaultSuite() is ModuleSuite[__FILE__]);


  // Test the observer methods
  void Unit_Observer(unit.unit_result result)
  {
    writefln("Unit: %s", result.toString());
  }
  DefaultUnitObserver(&Unit_Observer);
  void Suite_Observer(suite.suite_result result)
  {
    writefln("Suite: %s", result.toString());
  }
  DefaultSuiteObserver(&Suite_Observer);

  // Needs to be added to 'api.d' Suite
  test("Test 1 for " ~ __FILE__, "", {return false;});
  
  // Change the default suite
  DefaultSuite("not api.d"); // Only DefaultSuite(char[]) must be
			     // tested, because it calls
			     // DefaultSuite(suite)
  DefaultSuiteObserver(&Suite_Observer);
  test_except!(Exception)
    ("Test Exception for" ~ __FILE__, "", {throw new Exception("");return false;});
}

