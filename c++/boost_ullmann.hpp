// morpho/cdl/bgl_expansions/algorithms/ullmann.hpp header file//
// Copyright (c) 2003 Vladimir Josef Sykora and Morphochem AG
// vsyk at yahoo.com
//
// Permission to use, copy, modify, distribute and sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appears in all copies and
// that both the copyright notice and this permission notice appear in
// supporting documentation. No representations are made about the
// suitability of this software for any purpose. It is provided "as is"
// without express or implied warranty.
//-----------------------------------------------------------------------------

// Algorithm due to JR Ullmann. Journal of the Association for
// Computing Machinery, Vol 23, No.1, January 1976, pp 31-42.

#ifndef MORPHO_CDL_BGL_EXP_ULLMANN_HPP
#define MORPHO_CDL_BGL_EXP_ULLMANN_HPP

// -------- boost
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/graph/graph_traits.hpp>
// -------- std
#include <utility>    // for std::pair<>
#include <algorithm>

namespace boost {

  struct ullmann_throw {};

  namespace detail {

    template <class Graph, class UblasMatrix, class BackInsertionSequence, class EdgeLabeling>
    bool forward_checking(const Graph& g1, const Graph& g2,
    UblasMatrix& M, size_t count, BackInsertionSequence& F, size_t num_vert_g1,
    size_t num_vert_g2, EdgeLabeling& edge_labeling) {
      typedef std::pair<typename graph_traits<Graph>::edge_descriptor,bool> edge_presence;
      typename BackInsertionSequence::iterator   fi, fi_end=F.end();
      for (unsigned int k=count+1; k<num_vert_g1; ++k) {
        for (unsigned int l=0; l<num_vert_g2; ++l) {
          if(!M(k,l)) continue;
          // check mapping:
          fi=F.begin();
          while(fi!=fi_end) {
            bool flag1(0), flag1_1(0), flag2(0), flag2_1(0);
            edge_presence ep1=edge(k,fi->first,g1);
            if(ep1.second) {
              flag1_1=true;
              edge_presence ep2=edge(l,fi->second,g2);
              if(ep2.second) flag1=edge_labeling(ep1.first,ep2.first);
            }
            if(flag1_1 && flag1) {
              M(k,l)=1;
              ++fi;
              continue;
            }
            edge_presence ep2=edge(l,fi->second,g2);
            if(ep2.second) {
              flag2_1=true;
              ep1=edge(k,fi->first,g1);
              if(ep1.second) flag2=edge_labeling(ep1.first,ep2.first);
            }
            if(flag2_1 && flag2) {  //if one edge exists, there must be a mapping
              M(k,l)=1;
            }
            else if ( !flag1_1 && !flag2_1 ) { // or both edges are not present
              M(k,l)=1;
            }
            else M(k,l)=0;         // if not, there's no mapping
            ++fi;
          }
        }
      }
      // TODO: change the data structure of the M matrix to sparse matrix. This wouldn't be neccessary
      unsigned int cero_row(0);
      for (int k=0; k<num_vert_g1; ++k) {
        for (int l=0; l<num_vert_g2; ++l) {
          if(M(k,l)) break;
          else ++cero_row;
        }
        if(cero_row==num_vert_g2) return false; // if there is a cero row
        cero_row=0;
      }
      return true;
    }

    template <class Graph, class EdgeLabeling, class UblasMatrix, class BackInsertionSequence>
    void backtrack(const Graph& g1, const Graph& g2, size_t count,
    const UblasMatrix& M, BackInsertionSequence& F, const size_t num_vert_g1,
    const size_t num_vert_g2, EdgeLabeling& edge_labeling) {
      if(count==num_vert_g1) throw ullmann_throw();
      for (unsigned int i=0; i<num_vert_g2; ++i) {
        if(M(count,i)) {
          F.push_back(std::make_pair(count,i));
          UblasMatrix M_prime(M);
          for (unsigned int m=count+1; m<num_vert_g1; ++m) M_prime(m,i)=0;
          if (forward_checking(g1,g2,M_prime,count,F,num_vert_g1,num_vert_g2,edge_labeling))
            backtrack(g1,g2,count+1,M_prime,F,num_vert_g1,num_vert_g2,edge_labeling);
          F.erase(std::remove(F.begin(),F.end(),std::make_pair(count,i)), F.end());
        }
      }
    }

    template <class Graph, class EdgeLabeling, class UblasMatrix, class DoubleBackInsertionSequence>
    void backtrack_all(const Graph& g1, const Graph& g2, size_t count,
    const UblasMatrix& M, DoubleBackInsertionSequence& FF, const size_t num_vert_g1,
    const size_t num_vert_g2, EdgeLabeling& edge_labeling) {
      for (unsigned int i=0; i<num_vert_g2; ++i) {
        if(M(count,i)) {
          typename DoubleBackInsertionSequence::value_type  F;
          F.push_back(std::make_pair(count,i));
          UblasMatrix M_prime(M);
          for (unsigned int m=count+1; m<num_vert_g1; ++m) M_prime(m,i)=0;
          try {
          if (forward_checking(g1,g2,M_prime,count,F,num_vert_g1,num_vert_g2,edge_labeling))
            backtrack(g1,g2,count+1,M_prime,F,num_vert_g1,num_vert_g2,edge_labeling);
          F.erase(std::remove(F.begin(),F.end(),std::make_pair(count,i)), F.end());
          } catch(ullmann_throw) {
            FF.push_back(F);
            continue;
          }
        }
      }
    }


  }  // namespace detail


  // test if g1 is a subgraph of g2. mapped vertices are returned in F
  template <  class Graph
            , class VertexLabeling    // binary predicate
            , class EdgeLabeling      // binary predicate
            , class BackInsertionSequence   // contains std::pair<vertex_descriptor,vertex_descriptor>
  >
  bool ullmann(const Graph& g1, const Graph& g2,
  VertexLabeling& vertex_labeling, EdgeLabeling& edge_labeling, BackInsertionSequence& F) {
    typedef typename graph_traits<Graph>::vertex_descriptor    vertex_t;
    typedef ::boost::numeric::ublas::matrix<bool>   matrix_t;
    size_t rows(num_vertices(g1));
    size_t cols(num_vertices(g2));
    matrix_t M(rows,cols);
    // initialize the matrix:
    for (int i=0; i<rows; ++i)
      for (int j=0; j<cols; ++j)
        if(vertex_labeling(i,j)) M(i,j)=1;
    size_t    i(0);
    try {
      detail::backtrack(g1,g2,i,M,F,rows,cols,edge_labeling);
    } catch(ullmann_throw) {
      return true;
    }
    return false;
  }

  // test if g1 is a subgraph of g2.
  // F returns all mappings of g1 in g2. mapping in separate containers
  template <  class Graph
            , class VertexLabeling    // binary predicate
            , class EdgeLabeling      // binary predicate
            , class DoubleBackInsertionSequence   // contains a back insertion sequence
  >
  bool ullmann_all(const Graph& g1, const Graph& g2,
  VertexLabeling& vertex_labeling, EdgeLabeling& edge_labeling, DoubleBackInsertionSequence& F) {
    typedef typename graph_traits<Graph>::vertex_descriptor    vertex_t;
    typedef ::boost::numeric::ublas::matrix<bool>   matrix_t;
    size_t rows(num_vertices(g1));
    size_t cols(num_vertices(g2));
    matrix_t M(rows,cols);
    // initialize the matrix:
    for (int i=0; i<rows; ++i)
      for (int j=0; j<cols; ++j)
        if(vertex_labeling(i,j)) M(i,j)=1;
    size_t    i(0);
    detail::backtrack_all(g1,g2,i,M,F,rows,cols,edge_labeling);
    return !F.empty();
  }

}  // namespace boost


#endif MORPHO_CDL_BGL_EXP_ULLMANN_HPP



