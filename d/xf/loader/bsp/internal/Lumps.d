///
module xf.loader.bsp.internal.Lumps;

/**
	Below is an enum eLumps that holds all the lumps and their order in the file
*/

enum Lumps{
	Entities = 0, 		/// Stores player/object positions, etc...
	Textures,			/// Stores texture information
	Planes,				/// Stores the splitting planes
    Nodes,				/// Stores the BSP nodes
    Leafs,				/// Stores the leafs of the nodes
    LeafFaces,			/// Stores the leaf's indices into the faces
    LeafBrushes,		/// Stores the leaf's indices into the brushes
    Models,				/// Stores the info of world models
    Brushes,			/// Stores the brushes info (for collision)
    BrushSides,			/// Stores the brush surfaces info
    Vertices,			/// Stores the level vertices
    Indices,			/// Stores the level indices
    Shaders,			/// Stores the shader files (blending, anims..)
    Faces,				/// Stores the faces for the level
    Lightmaps,			/// Stores the lightmaps for the level
    LightVolumes,		/// Stores extra world lighting information
    VisData,			/// Stores PVS and cluster info (visibility)
    MaxLumps			/// A constant to store the number of lumps
}

/**
	Here is a lump structure.  The offset is the position into the file that 
	is the starting point of the current section.  The length is the number 
	of bytes that this lump stores. 

*/

struct BSPLump {
    int offset;///
    int length;///
}




