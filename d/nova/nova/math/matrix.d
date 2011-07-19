//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

  Acknowledgement:

    Initial implementation file has been grabbed from a digitalmars
    newsgroup, but I don't remember the authors name.

*/////////////////////////////////////////////////////////////////////////
module nova.math.matrix;

import tango.math.Math;

import Float = tango.text.convert.Float;

//////////////////////////////////////////////////////////////////////////
/**

  Vector template class, inherits from Matrix class

  Usage patterns:

  1. Template instance alias for convenience
  ---
  private alias Vector!(double) DoubleVector;
  private alias Matrix!(double) DoubleMatrix;
  ---

  2. Create vector instances
  ---
  DoubleVector a = new DoubleVector(1.0, 2.0, 3.0);
  DoubleVector b = new DoubleVector(2.0, 4.0, 6.0);
  ---

  3. Vector add
  ---
  DoubleVector c = a + b;
  ---

  4. Dot product
  ---
  DoubleMatrix m = new DoubleMatrix(3, 3, 2.0, 0.0, 0.0,
                                          0.0, 2.0, 0.0,
                                          0.0, 0.0, 2.0);

  DoubleVector d = m * a;
  ---

*/////////////////////////////////////////////////////////////////////////
class Vector(T) : Matrix!(T)
{
    this (int len)
    {
      _data.length = len;
      _nRows       = len;
      _nCols       = 1;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Constructor - creates a line vector

      Params:
        l    = The vector's size
        t    = The type of the vector to create
        ...  = Optional datas (0 or l values)

    */////////////////////////////////////////////////////////////////////////
    this (T[] newdata ...)
    {
      _data  = newdata;
      _nRows = newdata.length;
      _nCols = 1;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Duplicate the vector

      Returns: A newly allocated vector

    */////////////////////////////////////////////////////////////////////////
    Vector dup ()
    {
      Vector n = new Vector(_data.dup);
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Determine the length of the vector

    */////////////////////////////////////////////////////////////////////////
    int length()
    {
      return _data.length;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      add number to a column

    */////////////////////////////////////////////////////////////////////////
    void addRow(Matrix!(T) m, int row)
    {
      assert(row >= 0 && row < m.Rows);
      assert(this.length == m.Columns);

      for(int col = 0; col < this.length; col++)
      {
        _data[col] += m[col, row];
      }
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      determine minimum element of vector

    */////////////////////////////////////////////////////////////////////////
    T min()
    {
      assert( length > 0 );

      T min = _data[0];
      for(int i=1; i < length; i++)
      {
        if (_data[i] < min)
        {
          min = _data[i];
        }
      }
      return min;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      determine the index of the minimum element of vector

    */////////////////////////////////////////////////////////////////////////
    int minIdx()
    {
      assert( length > 0 );

      int min_idx = 0;
      T   min = _data[0];
      for(int i=1; i < length; i++)
      {
        if (_data[i] < min)
        {
          min     = _data[i];
          min_idx = i;
        }
      }
      return min_idx;
    }
}

//////////////////////////////////////////////////////////////////////////
/**

  Matrix template class

  Usage patterns:

  1. Template instance alias for convenience
  ---
  private alias Vector!(double) DoubleVector;
  private alias Matrix!(double) DoubleMatrix;
  ---

  2. Create vector instances
  ---
  DoubleVector a = new DoubleVector(1.0, 2.0, 3.0);
  DoubleVector b = new DoubleVector(2.0, 4.0, 6.0);
  ---

  3. Dot product
  ---
  DoubleMatrix m = new DoubleMatrix(3, 3, 2.0, 0.0, 0.0,
                                          0.0, 2.0, 0.0,
                                          0.0, 0.0, 2.0);

  DoubleVector d = m * a;
  ---

  4. Matrix multiplication
  ---
  DoubleMatrix n = new DoubleMatrix(3, 3,  1.0, 1.0, 0.0,
                                          -2.0, 3.0, 5.0,
                                           0.0, 1.0, 4.0);

  DoubleMatrix o = m * n;
  ---

*/////////////////////////////////////////////////////////////////////////
class Matrix(T)
{
    this (int l, int c, T[] values ...)
    {
      _nRows = l;
      _nCols = c;
      _data = values;
      int newlen = (_nRows * _nCols);
      int oldlen = _data.length;
      _data.length = newlen;
      // init rest to zero
      while (oldlen < newlen)
      {
        _data[oldlen] = 0.0;
        oldlen++;
      }
    }

    this (int l) { }
    this ()      { }

    //////////////////////////////////////////////////////////////////////////
    /**

      Duplicate the matrix

      Results: A newly allocated matrix

    */////////////////////////////////////////////////////////////////////////
    Matrix dup ()
    {
      Matrix n = new Matrix (_nRows, _nCols, _data.dup);
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Get a sub matrix by removing a line and a column.

      Params:
        l = The line index
        c = The column index

      Results: A new matrix

    */////////////////////////////////////////////////////////////////////////
    Matrix subMatrix (int l, int c)
    {
      assert (_nRows > 1 && _nCols > 1);
      Matrix n = new Matrix (_nRows-1, _nCols-1);
      for (int i = 0; i < _nRows-1; i++)
      {
        for (int j = 0; j < _nCols-1; j++)
        {
          if      ((i<l)  && (j<c))  n[i,j] = this[i,j];
          else if ((i<l)  && (j>=c)) n[i,j] = this[i,j+1];
          else if ((i>=l) && (j<c))  n[i,j] = this[i+1,j];
          else if ((i>=l) && (j>=c)) n[i,j] = this[i+1,j+1];
        }
      }
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Apply a function to all values of a matrix

      Params:
        fct = A function delegate

      Results: A new matrix

    */////////////////////////////////////////////////////////////////////////
    Matrix map (T delegate(T) fct)
    {
      Matrix n = new Matrix (_nRows, _nCols);
      for (int i = 0; i < _nCols*_nRows; i++)
        n[i] = fct(_data[i]);
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Get a particular vector from a column

      Params:
        c = Column

      Results: A new Vector

    */////////////////////////////////////////////////////////////////////////
    Vector!(T) getCol(uint c)
    {
      assert (c <= _nCols);
      Vector!(T) n = new Vector!(T)(_nRows);
      for (int e = 0; e < _nRows; e++) n[e] = this[e,c];
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Get a particular vector from a line

      Params:
        l = Line

      Results: A new Vector

    */////////////////////////////////////////////////////////////////////////
    Vector!(T) getRow(uint l)
    {
      assert (l <= _nRows);
      Vector!(T) n = new Vector!(T)(_nCols);
      for (int e = 0; e < _nCols; e++) n[e] = this[l,e];
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      get minimum of a row

    */////////////////////////////////////////////////////////////////////////
    T rowMin(int row)
    {
        int col;

        assert(row >= 0 && row < this.Rows);

        T min = this[row,0];
        for(col=1; col < _nCols; col++)
        {
          T v = this[row,col];
          if ( v < min)
          {
            min = v;
          }
        }
        return min;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      get minimum of a column

    */////////////////////////////////////////////////////////////////////////
    T colMin(int col)
    {
        int row;

        assert(col >= 0 && col < _nCols);

        T min = this[0,col];
        for(row=1; row < _nRows; row++)
        {
          T v = this[row,col];
          if ( v < min)
          {
            min = v;
          }
        }
        return min;
    }


    //////////////////////////////////////////////////////////////////////////
    /**

      set row values to given value

    */////////////////////////////////////////////////////////////////////////
    void setRow(int row, T value)
    {
       int col;

       assert( row >= 0 && row < this.Rows);
       for(col=0; col < _nCols; col++)
       {
         _data[row * _nCols + col] = value;
       }
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      subtract number from a column

    */////////////////////////////////////////////////////////////////////////
    void subRow(int row, T value)
    {
       int col;

       assert( row >= 0 && row < _nRows);
       for(col=0; col < _nCols; col++)
       {
         _data[row * _nCols + col] -= value;
       }
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      set column values to given value

    */////////////////////////////////////////////////////////////////////////
    void setCol(int col, T value)
    {
       int row;

       assert( col >= 0 && col < _nCols);

       for(row=0; row < _nRows; row++)
       {
         _data[row * _nCols + col] = value;
       }
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      subtract number from a row

    */////////////////////////////////////////////////////////////////////////
    void subCol(int col, T value)
    {
       int row;

       assert( col >= 0 && col < _nCols);

       for(row=0; row < _nRows; row++)
       {
         _data[row * _nCols + col] -= value;
       }
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Compute the determinant of the matrix

      Results: The determinant

    */////////////////////////////////////////////////////////////////////////
    T det()
    {
      assert (this.isSquare);
      if (_nRows == 2) return this[0,0]*this[1,1] - this[0,1]*this[1,0];
      if (_nRows == 1) return this[0,0];
      T det = this[0,0]* this.subMatrix(0,0).det;
      for (int i = 1; i < _nRows; i++)
      {
        if (i%2) det = det - this[i,0]*this.subMatrix(i,0).det;
        else     det = det + this[i,0]*this.subMatrix(i,0).det;
      }
      return det;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Compute the inverse of the matrix. It's determinant must be != 0.

      Results: A new matrix

    */////////////////////////////////////////////////////////////////////////
    Matrix inv()
    {
      T det = this.det;
      assert (det != 0);
      Matrix n = new Matrix (_nRows, _nCols);

      if (_nRows == 2)
      {
        n[0,0] =  1/det * this[1,1];
        n[0,1] = -1/det * this[0,1];
        n[1,0] = -1/det * this[1,0];
        n[1,1] =  1/det * this[0,0];
        return n;
      }
      for (int i = 0; i < _nRows; i++)
      {
        for (int j = 0; j < _nCols; j++)
        {
          if ((i+j)%2) n[j,i] = -this.subMatrix(i,j).det;
          else         n[j,i] =  this.subMatrix(i,j).det;
        }
      }
      return (1/det) * n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Compute the power p of the matrix

      Params:
        p = Power, must be > 0

      Results: A new matrix

    */////////////////////////////////////////////////////////////////////////
    Matrix pow (uint p)
    {
      assert (p != 0);
      if (p == 1) return this.dup;
      return this * this.pow(p-1);
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Fill the matrix with a value

      Params:
        val = the value

    */////////////////////////////////////////////////////////////////////////
    void fill  (T val)
    {
      for (int i = 0; i < _nRows*_nCols; i++) _data[i] = val;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Fill the main diagonal with a value

      Params:
        val - the value

    */////////////////////////////////////////////////////////////////////////
    void fillDiag(T val)
    {
      assert (_nRows == _nCols);
      for (int i = 0; i < _nRows; i++) this[i,i] = val;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Transpose the matrix

      Results: A new matrix

    */////////////////////////////////////////////////////////////////////////
    Matrix transpose ()
    {
      Matrix n = new Matrix (_nCols, _nRows);
      for (int i = 0; i < _nRows; i++)
      {
        for (int j = 0; j < _nCols; j++)
        {
          n[i,j] = this[j,i];
        }
      }
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Return the diagonal of the matrix as a vector

      Results: A new matrix

    */////////////////////////////////////////////////////////////////////////
    Vector!(T) diagonalize()
    {
      Vector!(T) n = new Vector!(T)(_nRows);
      for(int i=0; i < _nRows; i++)
      {
        n[i]  = this[i,i];
      }
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Trace of the matrix. (Diagonal sum)

      Results: Trace of the matrix. (Diagonal sum)

    */////////////////////////////////////////////////////////////////////////
    T trace ()
    {
      T tr = this[0];
      if (_nRows == _nCols) for (int e = 1; e < _nRows; e++) tr += this[e,e];
      return tr;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Print a matrix to stdout.

    */////////////////////////////////////////////////////////////////////////
    char[] print()
    {
      char[] result = "<";
      foreach (int i, T v; _data)
      {
        result ~= Float.toUtf8(v) ~ " ";
        if (!((1+i) % _nCols))
        {
          result ~= ">";
        }
      }
      return result;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Test if the matrix's dimensions are the same as m's.

    */////////////////////////////////////////////////////////////////////////
    int matchSize (Matrix m)
    {
      return ((m.Rows == _nRows) && (m.Columns == _nCols));
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Change the size of the matrix

      Params:
        l = new line number
        c = new col number

    */////////////////////////////////////////////////////////////////////////
    void setSize   (int l, int c)
    {
      _data.length = l*c;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Addition with assignation: A += B;

    */////////////////////////////////////////////////////////////////////////
    void opAddAssign (Matrix mat)
    {
      Matrix u = this + mat;
      _nCols   = u.Columns;
      _nRows   = u.Rows;
      _data    = u.Data;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Substraction with assignation: A -= B;

    */////////////////////////////////////////////////////////////////////////
    void opSubAssign (Matrix mat)
    {
      Matrix u = this - mat;
      _nCols   = u.Columns;
      _nRows   = u.Rows;
      _data    = u.Data;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Addition of two Matrix A + B

    */////////////////////////////////////////////////////////////////////////
    Matrix opAdd (Matrix mat)
    {
      assert (this.matchSize(mat));
      Matrix n = new Matrix (_nRows, _nCols);
      for (int i = 0; i < _nRows*_nCols; i++) n[i] = _data[i] + mat[i];
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Substraction of two Matrix A - B

    */////////////////////////////////////////////////////////////////////////
    Matrix opSub (Matrix mat)
    {
      assert (this.matchSize(mat));
      Matrix n = new Matrix (_nRows, _nCols);
      for (int i = 0; i < _nRows*_nCols; i++) n[i] = _data[i] - mat[i];
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Get a value: T a = M[l,c];

    */////////////////////////////////////////////////////////////////////////
    T opIndex(int l, int c)
  {
    // writef( "[", c, " ", l, " ", _nRows, " ", _data.length, " ", c + l*_nRows,"]");
    // writef(l, " ", c, " ",l + c*_nRows);
      return _data[l*_nCols + c];
    }
    T opIndex(int e)        { return _data[e];            };

    //////////////////////////////////////////////////////////////////////////
    /**

      Set a value: M[l,c] = d;

    */////////////////////////////////////////////////////////////////////////
    void opIndexAssign(T val, int l, int c) { _data[l*_nCols + c] = val; }
    void opIndexAssign(T val, int e)        { _data[e] = val;            }

    //////////////////////////////////////////////////////////////////////////
    /**

      Multiplication of two matrixes: A * B

    */////////////////////////////////////////////////////////////////////////
    Matrix opMul (Matrix mat)
    {
      assert (_nCols == mat.Rows);
      Matrix n = new Matrix (_nRows, mat.Columns);

      int i, j, k;
      for (i = 0; i < _nRows; i++)
      {
        for (j = 0; j < mat.Columns; j++)
        {
          for (k = 0; k < _nCols; k++)
          {
            n[i,j] = n[i,j] + this[i,k] * mat[k,j];
          }
        }
      }

      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Multiplication and assignation: A *= B

    */////////////////////////////////////////////////////////////////////////
    void opMulAssign (Matrix mat)
    {
      Matrix u = this * mat;
      _nCols = u.Columns;
      _nRows = u.Rows;
      _data = u.Data.dup;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Left scalar multiplication: s * A

    */////////////////////////////////////////////////////////////////////////
    Matrix opMul_r (T scal) { return opMul (scal); }

    //////////////////////////////////////////////////////////////////////////
    /**

      Right scalar multiplication: A * s

    */////////////////////////////////////////////////////////////////////////
    Matrix opMul (T scal)
    {
      Matrix n = new Matrix (_nRows, _nCols);
      for (int i = 0; i < _nRows*_nCols; i++) n[i] = _data[i] * scal;
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Scalar multiplication with assignation: A *= s;

    */////////////////////////////////////////////////////////////////////////
    void opMulAssign (T scal)
    {
      for (int i = 0; i < _nRows*_nCols; i++) _data[i] *= scal;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Test equality of two matrixes: A == B

    */////////////////////////////////////////////////////////////////////////
    bool opEquals (Matrix mat)
    {
      if (!this.matchSize(mat)) return false;
      for (int i = 0; i < _nRows*_nCols; i++) if (_data[i] != mat[i]) return false;
      return true;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Unary minus operator: -A

    */////////////////////////////////////////////////////////////////////////
    Matrix opNeg ()
    {
      Matrix n = new Matrix (_nRows, _nCols);
      for (int i = 0; i < _nRows*_nCols; i++) n[i] = - _data[i];
      return n;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      This enables foreach statements

    */////////////////////////////////////////////////////////////////////////
    int opApply (int delegate(inout T) dg)
    {
      int res = 0;
      for (int e = 0; e < _nRows*_nCols; e++)
      {
        res = dg(_data[e]);
        if (res) break;
      }
      return res;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Number of Rows.

    */////////////////////////////////////////////////////////////////////////
    int  Rows()            { return _nRows; }
    void Rows(int n)       { _nRows = n;    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Number of columns.

    */////////////////////////////////////////////////////////////////////////
    int  Columns()         { return _nCols; }
    void Columns(int c)    { _nCols = c;    }

    //////////////////////////////////////////////////////////////////////////
    /**

      True if the matrix is square, False otherwise.

    */////////////////////////////////////////////////////////////////////////
    int isSquare  ()      { return (_nRows == _nCols); }

    //////////////////////////////////////////////////////////////////////////
    /**



    */////////////////////////////////////////////////////////////////////////
    bool isZero()
    {
      foreach(T val; _data)
      {
        if (abs(val - NULL_COSTS) > EPS)
        {
          return false;
        }
      }

      return true;
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Data table access.

    */////////////////////////////////////////////////////////////////////////
    T[]  Data   ()         { return _data; }
    void Data   (T[] d)    { _data = d;    }

  public:

    const
    {
      T NULL_COSTS = 0;
      T INF_COSTS  = T.infinity;
      T EPS        = T.min;
    }

    int _nCols;
    int _nRows;
    T[] _data;
}

