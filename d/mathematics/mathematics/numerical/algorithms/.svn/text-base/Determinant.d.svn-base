/*


http://en.wikipedia.org/wiki/QR_decomposition#Connection_to_a_determinant_or_a_product_of_eigenvalues
*/

module mathematics.numerical.algorithms.Determinant;




private import mathematics.numerical.Matrix;
private import mathematics.numerical.algorithms.LuDecomposition;


T determinant(T)(Matrix!(T) matrix)
{
	assert( matrix.getRowCount() == matrix.getColumnCount(), "The matrix must be a square matrix");
	if (matrix.getRowCount() == 2)
	{
		return matrix[0,0]*matrix[1,1] - matrix[1,0]*matrix[1,1];
	} else if (matrix.getRowCount() == 3)
	{
	return matrix[0,0]*matrix[1,1]*matrix[2,2] 
		- matrix[0,0]*matrix[1,2]*matrix[2,1]
		+ matrix[0,1]*matrix[1,2]*matrix[2,0]
		- matrix[0,1]*matrix[1,0]*matrix[2,2]
		+ matrix[0,2]*matrix[1,0]*matrix[2,1]
		- matrix[0,2]*matrix[1,1]*matrix[2,0];		
	} else if (matrix.getRowCount() == 4)		//TODO test this one.
	{
		matrix[0,0]*matrix[1,1] *(matrix[2,2]*matrix[3,3]-matrix[3,2]*matrix[2,3]) 
- matrix[1,2] * (matrix[2,1]*matrix[3,3]-matrix[3,1]*matrix[2,3]) + matrix[1,3]*(matrix[2,1]*matrix[3,2]-matrix[3,1] * matrix[2,2]) - 

		matrix[0,1]* (matrix[1,0]*(matrix[2,2]*matrix[3,3]-matrix[3,2]*matrix[2,3]) 
- matrix[1,2] * (matrix[2,0]*matrix[3,3]-matrix[3,0]*matrix[2,3]) + matrix[1,3]*(matrix[2,0]*matrix[3,2]-matrix[3,0] * matrix[2,2])) + 

		matrix[0,2]* (matrix[1,0]*(matrix[2,1]*matrix[3,3]-matrix[3,1]*matrix[2,3])
 - matrix[1,1] * (matrix[2,0]*matrix[3,3]-matrix[3,0]*matrix[2,3]) + matrix[1,3]*(matrix[2,0]*matrix[3,1]-matrix[3,0] * matrix[2,1])) - 

		matrix[0,3]* (matrix[1,0]*(matrix[2,1]*matrix[3,2]-matrix[3,1]*matrix[2,2])
 - matrix[1,1] * (matrix[2,0]*matrix[3,2]-matrix[3,0]*matrix[2,2]) + matrix[1,2]
*(matrix[2,0]*matrix[3,1]-matrix[3,0] * matrix[2,1]));
	} else if (matrix.getRowCount() == 1)
	{
		return matrix[0,0];
	} 


	

	/*
		else PLU factorization, multiplication of the diagonal of U, and sign of P, even + sign, odd -.
		DGEMM with alpha set to 1 and beta 0. 
	*/
	return -1;
}

