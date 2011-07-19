module xf.maths.Bezier;

private {
	import xf.utils.RectArray;
}



static void tesselateBezierPatch(Vertex)(RectArray!(Vertex) cp, RectArray!(Vertex) vertices) {
	assert (cp.width == 3 && cp.height == 3);
	assert (vertices.width >= 2 && vertices.height >= 2);
	
	float b0(float t)	{ return (1f - t) * (1f - t); }
	float b1(float t)	{ return (1f - t) * t * 2f; }
	float b2(float t)	{ return t * t; }
	
	
	for (int x = 0; x < vertices.width; ++x) {
		float px = cast(float)x / (vertices.width - 1);
		vertices[x, 0] = cp[0, 0] * b0(px) + cp[1, 0] * b1(px) + cp[2, 0] * b2(px);
	}
	
	for (int y = 0; y < vertices.height; ++y) {
		float py = cast(float)y / (vertices.height - 1);
		
		Vertex tmp0 = cp[0, 0] * b0(py) + cp[0, 1] * b1(py) + cp[0, 2] * b2(py);
		Vertex tmp1 = cp[1, 0] * b0(py) + cp[1, 1] * b1(py) + cp[1, 2] * b2(py);
		Vertex tmp2 = cp[2, 0] * b0(py) + cp[2, 1] * b1(py) + cp[2, 2] * b2(py);
		
		for (int x = 0; x < vertices.width; ++x) {
			float px = cast(float)x / (vertices.width - 1);
			vertices[x, y] = tmp0 * b0(px) + tmp1 * b1(px) + tmp2 * b2(px);
		}
	}
}
