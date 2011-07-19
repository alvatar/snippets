//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.math.unittests.matrix;

private import tango.util.log.Log;
private import tango.text.convert.Sprint;
private import tango.io.Stdout;

private Logger logger;
static this()
{
    logger = Log.getLogger("unittests.matrix");
}

private import nova.math.matrix;

private alias Vector!(double) DoubleVector;
private alias Matrix!(double) DoubleMatrix;

unittest
{
  logger.info("Starting Vector/Matrix unittest");

  DoubleVector a = new DoubleVector(1.0, 2.0, 3.0);
  DoubleVector b = new DoubleVector(2.0, 4.0, 6.0);

  DoubleMatrix c = new DoubleMatrix(3, 3, 2.0, 0.0, 0.0,
                                          0.0, 2.0, 0.0,
                                          0.0, 0.0, 2.0);


  DoubleMatrix e = new DoubleMatrix(3, 3, 1.0, 0.0, 0.0,
                                          0.0, 1.0, 0.0,
                                          0.0, 0.0, 1.0);

  DoubleMatrix f = new DoubleMatrix(2, 3, 1.0, 4.0,  2.0,
                                          4.0, 0.0, -3.0);

  DoubleMatrix g = new DoubleMatrix(3, 3,  1.0, 1.0, 0.0,
                                          -2.0, 3.0, 5.0,
                                           0.0, 1.0, 4.0);

  DoubleMatrix r = new DoubleMatrix(2, 3, -7.0, 15.0,  28.0,
                                           4.0,  1.0, -12.0);

  // check opIndex
  assert(f[0,0] ==  1.0);
  assert(f[0,1] ==  4.0);
  assert(f[0,2] ==  2.0);
  assert(f[1,0] ==  4.0);
  assert(f[1,1] ==  0.0);
  assert(f[1,2] == -3.0);

  assert(f[0,0] == f[0]);
  assert(f[0,1] == f[1]);
  assert(f[0,2] == f[2]);
  assert(f[1,0] == f[3]);
  assert(f[1,1] == f[4]);
  assert(f[1,2] == f[5]);

  // logger.info(f.print());

  DoubleMatrix h = f * g;
  // logger.info(h.print());
  // logger.info(r.print());

  assert(h == r);

  DoubleMatrix x = new DoubleMatrix();

  // try to print empty matrix
  // logger.info(x.print());

  x = c * e;

  assert(x == c);

  x = c * a;

  assert(x == b);

  x = e *a;

  assert(x == a);

  logger.info("Done");
}

