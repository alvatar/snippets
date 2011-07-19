module Serialize;

import utill;
import XML;

public import Interface;
public import Marshal;
public import Demarshal;
public import Common;

/**
 * Mixin template that attempt to "do the right" thing for general cases
 *  
 * Authors: Benjamin
 */
template Serializable()
{
	const bool CyclicType = false;

	mixin MarshalMixin!();
	mixin DemarshalMixin!();
	mixin CommonMixin!();
}

/**
 * Mixin template that attempt to "do the right" thing for general cases
 * Handels repeated instances of the same object (hard links)
 *  
 * Authors: Benjamin
 */
template SerializableRecurring()
{
	const bool CyclicType = true;

	mixin MarshalMixin!();
	mixin DemarshalMixin!();
	mixin CommonMixin!();
}
