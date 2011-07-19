module xf.loader.scene.hme.HmeMaterial;

private {
	import xf.loader.scene.model.Material;
	import xf.omg.core.LinearAlgebra;
}


struct Map {
	float		amount		= 0.f;
	char[]	texture;
	bool		enabled		= false;
}


class Standard3dsMaxMaterial : Material {
	Map	diffuseMap,
			ambientMap,
			specularMap,
			glossinessMap,
			selfIllumMap,
			opacityMap,
			filterMap,
			bumpMap,
			reflectionMap,
			refractionMap,
			displacementMap;
	
	char[] shaderByName;
	
	float	opacity = 1.f;
	
	vec3	diffuse		= {r: 1.f, g: 1.f, b: 1.f};
	vec3	ambient	= {r: 0.f, g: 0.f, b: 0.f};
	vec3	specular	= {r: 1.f, g: 1.f, b: 1.f};
	
	float	glossiness	= 0.1f;
	float	specularLevel = 0.f;
	
	float	selfIllumAmount	= 0.f;
	vec3	selfIllumColor	= {r: 1.f, g: 1.f, b: 1.f};
}
