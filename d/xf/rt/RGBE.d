module xf.rt.RGBE;

private {
	import tango.stdc.stdio : FILE;
}



extern (C) {
	struct rgbe_header_info {
	  int valid;            /* indicate which fields are valid */
	  char programtype[16]; /* listed at beginning of file to identify it 
							 * after "#?".  defaults to "RGBE" */ 
	  float gamma;          /* image has already been gamma corrected with 
							 * given gamma.  defaults to 1.0 (no correction) */
	  float exposure;       /* a value of 1.0 in an image corresponds to
				 * <exposure> watts/steradian/m^2. 
				 * defaults to 1.0 */
	}


	int RGBE_WriteHeader(FILE *fp, int width, int height, rgbe_header_info *info);
	int RGBE_ReadHeader(FILE *fp, int *width, int *height, rgbe_header_info *info);

	/* read or write pixels */
	/* can read or write pixels in chunks of any size including single pixels*/
	int RGBE_WritePixels(FILE *fp, float *data, int numpixels);
	int RGBE_ReadPixels(FILE *fp, float *data, int numpixels);

	/* read or write run length encoded files */
	/* must be called to read or write whole scanlines */
	int RGBE_WritePixels_RLE(FILE *fp, float *data, int scanline_width,
				 int num_scanlines);
	int RGBE_ReadPixels_RLE(FILE *fp, float *data, int scanline_width,
				int num_scanlines);
}
