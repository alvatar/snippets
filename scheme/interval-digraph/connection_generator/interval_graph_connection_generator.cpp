/*
 *  interval_graph_connection_generator.cpp
 *
 *  Connection generator interface to the Chicken Scheme interval
 *  graph library.
 * 
 *  Adapted by Ivan Raikov from the NEST connection generator library.
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

#include <assert.h>
#include <chicken.h>

#include "interval_graph_connection_generator.h"



typedef C_word value;

#define Val_return(v)         C_return(v)
#define Val_unit              C_SCHEME_UNDEFINED

#define Val_to_double(v)       C_c_double(v)

#define Val_is_truep(v)       C_truep(v)
#define Val_is_null_listp(v)  C_i_null_list_p(v)

#define Val_list_length(v)    C_i_length(v)
#define Val_car(v)            C_u_i_car(v)
#define Val_cdr(v)            C_u_i_cdr(v)


void chicken_print_error ()
{
     char *msg;
     msg = (char *)malloc(512);
     CHICKEN_get_error_message (msg, 511);
     fprintf(stderr, "CHICKEN error: %s\n", msg);
}



/* A flag that indicates whether the Scheme system has been initialized */
static int scheme_initialized = 0;

/* The size of the vector containing interval-digraph procedures. */
#define idg_ndata 5

/* The size of the vector containing cis procedures. */
#define cis_ndata 15

/* A vector with pointers to the Scheme interval-digraph
 * procedures. This is necessary in order to prevent the garbage
 * collector from moving the procedures in memory and changing their
 * entry addresses. (See call to C_gc_protect below). */ 

value *idg_data[ idg_ndata ];

static value idg_size                      = Val_unit;
static value idg_succ_interval             = Val_unit;
static value idg_arity                     = Val_unit;
static value idg_edge_property_list_map    = Val_unit;
static value idg_edge_property_get         = Val_unit;


/* A vector with pointers to the Scheme cis procedures. This is
 * necessary in order to prevent the garbage collector from moving
 * the procedures in memory and changing their entry addresses. (See
 * call to C_gc_protect below). */ 

value *cis_data[ cis_ndata ];

static value cis_empty     = Val_unit;
static value cis_singleton = Val_unit;
static value cis_interval  = Val_unit;
static value cis_is_empty  = Val_unit;
static value cis_is_subset = Val_unit;
static value cis_cardinal  = Val_unit;
static value cis_get_min   = Val_unit;
static value cis_get_max   = Val_unit;
static value cis_in        = Val_unit;
static value cis_add       = Val_unit;
static value cis_remove    = Val_unit;
static value cis_shift     = Val_unit;

static value cis_union            = Val_unit;
static value cis_intersection     = Val_unit;
static value cis_difference       = Val_unit;


/* Initializes the cis and interval-digraph libraries, allocates pointers to its procedures. */
int scheme_initialize ()
{
    int status; value val;
    
   if (scheme_initialized == 0)
   {
    assert (status = CHICKEN_initialize (0, 0, 0, CHICKEN_default_toplevel));
    assert (status = CHICKEN_run (NULL));

    assert (status = CHICKEN_eval_string ("(require-extension cis)", &val));
    assert (status = CHICKEN_eval_string ("(require-extension interval-digraph)", &val));

    assert (status = CHICKEN_eval_string ("empty", &cis_empty));
    assert (status = CHICKEN_eval_string ("(lambda (x) (singleton x) )", &cis_singleton));
    assert (status = CHICKEN_eval_string ("(lambda (x y) (interval x y))", &cis_interval));
    assert (status = CHICKEN_eval_string ("(lambda (x)  (empty? x))", &cis_is_empty));
    assert (status = CHICKEN_eval_string ("(lambda (x y)  (subset? x y))", &cis_is_subset));
    assert (status = CHICKEN_eval_string ("(lambda (x)  (cardinal x ))", &cis_cardinal));
    assert (status = CHICKEN_eval_string ("(lambda (x)  (get-min x ))", &cis_get_min));
    assert (status = CHICKEN_eval_string ("(lambda (x)  (get-max x ))", &cis_get_max));
    assert (status = CHICKEN_eval_string ("(lambda (i x)  (in? i x ))", &cis_in));
    assert (status = CHICKEN_eval_string ("(lambda (i x)  (add i x ))", &cis_add));
    assert (status = CHICKEN_eval_string ("(lambda (i x)  (remove i x ))", &cis_remove));
    assert (status = CHICKEN_eval_string ("(lambda (i x)  (shift i x ))", &cis_shift));

    assert (status = CHICKEN_eval_string ("(lambda (x y)  (union x y ))", &cis_union));
    assert (status = CHICKEN_eval_string ("(lambda (x y)  (intersection x y ))", &cis_intersection));
    assert (status = CHICKEN_eval_string ("(lambda (x y)  (difference x y ))", &cis_difference));

    assert (status = CHICKEN_eval_string ("(lambda (x)  ((x 'size)))", &idg_size));
    assert (status = CHICKEN_eval_string ("(lambda (i x)  ((x 'succ-interval) i))", &idg_succ_interval));
    assert (status = CHICKEN_eval_string ("(lambda (x)  (length (x 'edge-property-keys)))", 
					  &idg_arity));
    assert (status = CHICKEN_eval_string ("(lambda (i x) (gc) ((x 'edge-property-list-map) i))", 
					  &idg_edge_property_list_map));
    assert (status = CHICKEN_eval_string ("(lambda (j lst)  (map (lambda (p) (or (and (cdr p) (let ((prop-map (cdr p))) ((prop-map 'get-value) j 0.0))) 0.0)) lst))",
					  &idg_edge_property_get));



    cis_data[0]  = &cis_empty;
    cis_data[1]  = &cis_singleton;
    cis_data[2]  = &cis_interval;
    cis_data[3]  = &cis_is_empty;
    cis_data[4]  = &cis_is_subset;
    cis_data[5]  = &cis_cardinal;
    cis_data[6]  = &cis_get_min;
    cis_data[7]  = &cis_get_max;
    cis_data[8]  = &cis_in;
    cis_data[9]  = &cis_add;
    cis_data[10] = &cis_remove;
    cis_data[11] = &cis_shift;

    cis_data[12] = &cis_union;
    cis_data[13] = &cis_intersection;
    cis_data[14] = &cis_difference;

    idg_data[0] = &idg_size;
    idg_data[1] = &idg_succ_interval;
    idg_data[2] = &idg_arity;
    idg_data[3] = &idg_edge_property_list_map;
    idg_data[4] = &idg_edge_property_get;

    C_gc_protect(cis_data, cis_ndata);
    C_gc_protect(idg_data, idg_ndata);

    scheme_initialized = 1;

   }

   return status;
}


static void release_gc_root (void* v) 
{
     assert (scheme_initialized);
     CHICKEN_delete_gc_root (v);
}

static void* make_cis (int x, int y)
{
     value is, list, res, *ptr;
     int status; void *root;

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(2));
     list = C_list(&ptr, 2, C_fix(x), C_fix(y));
	  
     assert (status = CHICKEN_apply (cis_interval, list, &is));

     root = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (root, is);

     return root;
}



static void* c_cis_empty ()
{
     value res; void *root; int status; 

     res = cis_empty;

     root = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (root, res);

     return root;
}

static int c_cis_isempty (void *x)
{
     value res, list, *ptr;   int status; 

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(1));
     list = C_list(&ptr, 1, CHICKEN_gc_root_ref(x));
	  
     assert (status = CHICKEN_apply (cis_is_empty, list, &res));

     return C_truep(res);
}


static int c_cis_getmin (void *x)
{
     value res, list, *ptr;   int status; 

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(1));
     list = C_list(&ptr, 1, CHICKEN_gc_root_ref(x));
	  
     assert (status = CHICKEN_apply (cis_get_min, list, &res));

     return C_unfix(res);
}



static void* c_cis_remove (unsigned int x, void *t)
{
     value res, list, *ptr;   int status;  void *root;

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(2));
     list = C_list(&ptr, 2, C_fix(x), CHICKEN_gc_root_ref(t));
	  
     assert (status = CHICKEN_apply (cis_remove, list, &res));

     root = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (root, res);

     return root;
}



static void* c_cis_union (void *x, void *y)
{
     value res, list, *ptr;
     int status; void *root;

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(2));
     list = C_list(&ptr, 2, CHICKEN_gc_root_ref(x), CHICKEN_gc_root_ref(y));
	  
     assert (status = CHICKEN_apply (cis_union, list, &res));

     root = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (root, res);

     return root;
}


static void* c_cis_intersection (void *x, void *y)
{
     value res, list, *ptr;
     int status; void *root;

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(2));
     list = C_list(&ptr, 2, CHICKEN_gc_root_ref(x), CHICKEN_gc_root_ref(y));
	  
     assert (status = CHICKEN_apply (cis_intersection, list, &res));

     root = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (root, res);

     return root;
}

static int c_idg_size (void *x)
{
     value res, list, *ptr; int status; 

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(1));
     list = C_list(&ptr, 1, CHICKEN_gc_root_ref(x));
	  
     assert (status = CHICKEN_apply (idg_size, list, &res));

     return C_unfix(res);
}


static void* c_idg_succ_interval (int i, void *x)
{
     value res, list, *ptr; void *root; int status; 

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(2));
     list = C_list(&ptr, 2, C_fix(i), CHICKEN_gc_root_ref(x));
	  
     assert (status = CHICKEN_apply (idg_succ_interval, list, &res));

     root = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (root, res);

     return root;
}


static int c_idg_arity (void *x)
{
     value res, list, *ptr; int status; 

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(1));
     list = C_list(&ptr, 1, CHICKEN_gc_root_ref(x));
	  
     assert (status = CHICKEN_apply (idg_arity, list, &res));

     return C_unfix(res);
}


static void* c_idg_edge_property_list_map (int i, void *x)
{
     value res, list, *ptr; void *root; int status; 

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(2));
     list = C_list(&ptr, 2, C_fix(i), CHICKEN_gc_root_ref(x));
	  
     assert (status = CHICKEN_apply (idg_edge_property_list_map, list, &res));

     root = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (root, res);

     return root;
}


static void c_idg_edge_property_get (int j, void *map, double *values)
{
     value res, list, *ptr, l; int status, i; 

     assert (scheme_initialized);

     ptr = C_alloc (C_SIZEOF_LIST(2));
     list = C_list(&ptr, 2, C_fix(j), CHICKEN_gc_root_ref(map));
	  
     assert (status = CHICKEN_apply (idg_edge_property_get, list, &res));

     for (i = 0, l = res; !(Val_is_truep(Val_is_null_listp(l))); i++, l = Val_cdr(l)) 
     {
	  values[i] = Val_to_double(Val_car(l));
     }
}



//
// TODO: what is the local flag on setMask?
//

IntervalGraphConnectionGenerator::IntervalGraphConnectionGenerator(value v)
{
     igraph = CHICKEN_new_gc_root ();
     CHICKEN_gc_root_set (igraph, v);

     source_iset             = c_cis_empty ();
     target_iset             = c_cis_empty ();
     remaining_source_iset   = NULL;
     cset                    = NULL;
     source_iset_index       = -1;
     cset_index              = -1;
     pmap                    = NULL;
}


int IntervalGraphConnectionGenerator::size ()
{
     return c_idg_size (igraph);
}

int IntervalGraphConnectionGenerator::arity()
{

     return c_idg_arity (igraph);
}


void IntervalGraphConnectionGenerator::setMask (Mask& mask) 
{
     std::vector<ClosedInterval>::iterator interit;

     IntervalSet sources = mask.sources;
     for (interit = sources.begin();
	  interit < sources.end();
	  interit++)
     {
	  ClosedInterval inter = *interit;
	  
	  void *is = make_cis (inter.first, inter.last);
	  source_iset = c_cis_union (is, source_iset);
	  
	  release_gc_root (is);
     }
     
     IntervalSet targets = mask.targets;
     for (interit = targets.begin();
	  interit < targets.end();
	  interit++)
     {
	  ClosedInterval inter = *interit;
	  
	  void *is = make_cis (inter.first, inter.last);
	  
	  target_iset = c_cis_union (is, target_iset);
	  
	  release_gc_root (is);
     }
}

void IntervalGraphConnectionGenerator::setMask (std::vector<Mask>& masks, int local) 
{ 
     std::vector<Mask>::iterator maskit;
     std::vector<ClosedInterval>::iterator interit;

     for (maskit = masks.begin(); 
	  maskit < masks.end();
	  maskit++)
     {
	  Mask mask = *maskit;
	  IntervalSet sources = mask.sources;
	  for (interit = sources.begin();
	       interit < sources.end();
	       interit++)
	  {
	       ClosedInterval inter = *interit;

	       void *is = make_cis (inter.first, inter.last);
	       
	       source_iset = c_cis_union (is, source_iset);

	       release_gc_root (is);
	  }
	  IntervalSet targets = mask.targets;
	  for (interit = targets.begin();
	       interit < targets.end();
	       interit++)
	  {
	       ClosedInterval inter = *interit;

	       void *is = make_cis (inter.first, inter.last);
	       
	       target_iset = c_cis_union (is, target_iset);

	       release_gc_root (is);
	  }
     }
}


void IntervalGraphConnectionGenerator::start () 
{ 
     if (remaining_source_iset != NULL)
     {
	  release_gc_root (remaining_source_iset);
	  remaining_source_iset = NULL;
     }
     if (cset != NULL)
     {
	  release_gc_root (cset);
	  cset = NULL;
     }

     remaining_source_iset = source_iset;

     pmap = NULL;
     cset = NULL;
     cset_index = -1;
     source_iset_index = -1;
}

bool IntervalGraphConnectionGenerator::next (int& source, int& target, double* value) 
{
     void *succ_iset;
     
     if ((cset == NULL) || (c_cis_isempty (cset)))
     {
	  if (c_cis_isempty (remaining_source_iset))
	  {
	       return false;
	  }
	  else
	  {
	       if (pmap != NULL) release_gc_root (pmap);
	       do 
	       {
		    source_iset_index = c_cis_getmin (remaining_source_iset);
		    remaining_source_iset = c_cis_remove (source_iset_index, remaining_source_iset);
		    
		    succ_iset = c_idg_succ_interval (source_iset_index, igraph);

	       } while ((c_cis_isempty (succ_iset)) && (!(c_cis_isempty (remaining_source_iset))));


	       if (c_cis_isempty (remaining_source_iset)) return false;

	       cset = c_cis_intersection (succ_iset, target_iset);

	       release_gc_root (succ_iset);
	       pmap = c_idg_edge_property_list_map (source_iset_index, igraph);
	  }
     }
     
     cset_index = c_cis_getmin (cset);
     cset = c_cis_remove (cset_index, cset);
     
     source = source_iset_index;
     target = cset_index;
 
     c_idg_edge_property_get (target, pmap, value);

     return true;
}
