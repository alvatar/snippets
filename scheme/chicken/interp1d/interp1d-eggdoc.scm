
(use eggdoc)

(define doc
  `((eggdoc:begin
     (name "interp1d")
     (description "One-dimensional numerical interpolation.")
     (author (url "http://chicken.wiki.br/users/ivan-raikov" "Ivan Raikov"))

     (history 
      (version "1.10" "Ported to Chicken 4")      
      (version "1.9" "Removed testeez as a dependency")      
      (version "1.8" "Added lbound and ubound routines")      
      (version "1.7" "Added nearest interpolation method")      
      (version "1.6" "Bug fix in bounds routine")      
      (version "1.5" "Bug fix in the sliding window routine for the case of non-list input data")      
      (version "1.4" "Added a proper error message routine")      
      (version "1.3" "Added a boundary exceeded error message to the sliding window routine")      
      (version "1.2" "Build script updated for better cross-platform compatibility")      
      (version "1.1" "Documentation updates")
      (version "1.0" "Initial release"))

     (requires easyffi)

     (usage "(require-extension interp1d)")

     (download "interp1d.egg")

     (documentation
      
      (p "The " (tt "interp1d") " library provides routines for linear and piecewise "
	 "quadratic interpolation, as well as a facility for \"sliding window\" interpolation  "
	 "over a long, or possibly infinite, sequence of data. ")

      (subsection "Procedures"

		  (procedure "interp1d:nearest:: XDATA * YDATA * X  -> Y"
			     (p "Interpolates function " (tt "y=f(x)") " at the point "
				(tt "x") " using the data point nearest to " (tt "x") ". "
				"Arguments " (tt "XDATA") " and " (tt "YDATA") " are lists of "
				"numeric values that correspond to sample points of the function being interpolated. "
				"Argument " (tt "X") " must be within the range of values contained in " 
				(tt "XDATA") ". "))

		  (procedure "interp1d:linear:: XDATA * YDATA * X  -> Y"
			     (p "Interpolates function " (tt "y=f(x)") " at the point "
				(tt "x") " using the linear interpolation method. "
				"Arguments " (tt "XDATA") " and " (tt "YDATA") " are lists of "
				"numeric values that correspond to sample points of the function being interpolated. "
				"Argument " (tt "X") " must be within the range of values contained in " 
				(tt "XDATA") ". "))

		  (procedure "interp1d:piecewise-quadratic:: XDATA * YDATA * X  -> Y"
			     (p "Interpolates function " (tt "y=f(x)") " at the point "
				(tt "x") " using the piecewise quadratic interpolation method. "
				"Arguments " (tt "XDATA") " and " (tt "YDATA") " are lists of "
				"numeric values that corresponds to sample points of the function being interpolated. "
				"Argument " (tt "X") " must be within the range of values contained in " 
				(tt "XDATA") ". "))

		  (procedure "interp1d:from-sequence:: METHOD * STEP * YDATA [* START * CAR * CDR * NULL? *
		                                                              DELTA-WINDOW-LEN * MAX-WINDOW-LEN] ->
                                                       (LAMBDA X -> Y)"
			     ((p "Constructs a sliding window interpolation procedure, for a given sequence " (tt "YDATA")
			       " that contains uniformly sampled values of a function " (tt "y = f(x)") 
			       ", where " (tt "X") " has an initial value of " (tt "START") " (default 0) " 
			       " and is incremented by " (tt "STEP")  " amount. "
			       "The returned procedure performs interpolation using the given " (tt "METHOD") 
			       " procedure, while the window is shifted forwards by the appropriate amount if "
			       (tt "X") " is outside the current window range. Backward shifting is not supported. ")
			      (p "Argument " (tt "METHOD") " is one of the interpolation procedures above. ")
			      (p "Optional argument " (tt "START") " specifies the initial (smallest) " (tt "X") " and "
				 "defaults to 0. ")
			      (p "Optional arguments " (tt "CAR") ", " (tt "CDR") ", " (tt "NULL?") " are used "
				 "to access " (tt "YDATA") " and default to the standard list procedures. "
				 "If the input sequence is not a list (e.g. an SRFI-40 stream), these arguments "
				 "can be used to specify the appropriate accessors and predicates. ")
			      (p "Optional arguments " (tt "DELTA-WINDOW-LEN") " and " (tt "MAX-WINDOW-LEN")  " "
				 "specify the window increment (default 4) and maximum window size (default 128). ")))

		  
		  (procedure "interp1d:lbound:: XDATA * YDATA * X  -> Y"
			     (p "Returns the value of the data point nearest to " 
				(tt "X") " that does not exceed the value of "  (tt "X") ". "))

		  (procedure "interp1d:ubound:: XDATA * YDATA * X  -> Y"
			     (p "Returns the value of the data point nearest to " 
				(tt "X") " that is larger than "  (tt "X") ". "))


		  )

     (examples)

     (license
      "Copyright 2007-2009 Ivan Raikov and the Okinawa Institute of Science and Technology.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

A full copy of the GPL license can be found at
<http://www.gnu.org/licenses/>.")))))

(if (eggdoc->html doc) (void))
