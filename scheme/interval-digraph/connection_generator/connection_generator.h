/*
 *  connection_generator.h
 *
 *  This file is part of NEST
 *
 *  Copyright (C) 2011 by
 *  The NEST Initiative
 *
 *  See the file AUTHORS for details.
 *
 *  Permission is granted to compile and modify
 *  this file for non-commercial use.
 *  See the file LICENSE for details.
 *
 */

#ifndef CONNECTION_ITERATOR_H
#define CONNECTION_ITERATOR_H

#define CONNECTION_ITERATOR_DEBUG 1

#include <vector>

/**
 * Pure abstract base class for connection generators.
 */
class ConnectionGenerator {
 public:

  class ClosedInterval {
  public:
    ClosedInterval (int _first, int _last) : first (_first), last (_last) { }
    int first;
    int last;
  };

  class IntervalSet {
    std::vector<ClosedInterval> ivals;
    int _skip;
  public:
    IntervalSet (int skip = 1) : ivals (), _skip (skip) { }
    typedef std::vector<ClosedInterval>::iterator iterator;
    iterator begin () { return ivals.begin (); }
    iterator end () { return ivals.end (); }
    int skip () { return _skip; }
    void setSkip (int skip) { _skip = skip; }
    void insert (int first, int last) {
      ivals.push_back (ClosedInterval (first, last));
    }
  };

  class Mask {
  public:
    Mask (int sourceSkip = 1, int targetSkip = 1)

      : sources (sourceSkip), targets (targetSkip) { }
    IntervalSet sources;
    IntervalSet targets;
  };


  /**
   * Return the number of values associated with this iterator
   *  virtual int arity () = 0;

  /**
   * Inform the generator of which source and target indexes exist
   * (must always be called before any of the methods below)
   *
   * skip can be used in round-robin allocation schemes.
   */
  virtual void setMask (Mask& mask) = 0;

  /**
   * For a parallel simulator, we want to know the masks for all ranks
   */
  virtual void setMask (std::vector<Mask>& masks, int local) = 0;

  /**
   * Return number of connections represented by this iterator
   */
  virtual int size () = 0;

  /**
   * Start an iteration (must be called before first next)
   */
  virtual void start () = 0;

  /**
   * Advance to next connection or return false
   */
  virtual bool next (int& source, int& target, double* value) = 0;
};

#ifdef CONNECTION_ITERATOR_DEBUG
ConnectionGenerator* makeDummyConnectionIterator ();
#endif

#endif /* #ifndef CONNECTION_ITERATOR_H */
