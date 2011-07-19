global out
global dumpPath
global renderTextureSize
global numSceneObjects = 0
global curSceneObject = 0


fn exportProps obj props =
(
	for p in props do (
		if isProperty obj p then (
			format "% = " p to:out

			prop = getProperty obj p
			cls = classof prop

			if cls == Color then (
				format "% % %\n" prop.r prop.g prop.b to:out
			) else if cls == String then (
				format "'%'\n" prop to:out
			) else if cls == Point then (
				format "% % %\n" prop.x prop.z -prop.y to:out
			) else if cls == Quat then (
				format "% % % %\n" prop.x prop.z -prop.y prop.w to:out		-- TODO
			) else (
				format "%\n" prop to:out
			)
		)
	)
)


fn exportNodeTransform nodeXform refFrame =
(
	if (refFrame == undefined) then (
		refFrame = matrix3 1
	) else (
		refFrame = inverse((refFrame.transform.rotationPart as matrix3) * (transMatrix refFrame.transform.translationPart))
	)

	format "{\n" to:out


	if isProperty nodeXform "pos" then (
		pos = (nodeXform * refFrame).pos
		format "pos = % % %\n" pos.x pos.z -pos.y to:out
	)

	if isProperty nodeXform "rotation" then (
		rot = (nodeXform * refFrame).rotation
		format "rot = % % % %\n" rot.x rot.z -rot.y rot.w to:out
	)

	if isProperty nodeXform "scale" then (
		scl = (nodeXform * refFrame).scale
		-- format "scl = % % %\n" scl.x scl.z scl.y to:out
		
		-- scaling is now baked into mesh transforms, thus scl will always be unit
		format "scl = 1 1 1\n" to:out
	)

	format "}\n" to:out
)


fn exportMesh mesh reverseWinding =
(
	format "mesh = {\n" to:out

	numVerts = mesh.numVerts
	format "verts = % {\n" numVerts to:out
	for v = 1 to numVerts do (
		vert = getVert mesh v
		format "% % %\n" vert.x vert.z -vert.y to:out
	)
	format "}\n" to:out


	numFaces = mesh.numFaces
	format "faces = % {\n" numFaces to:out
	for f = 1 to numFaces do (
		face = (getFace mesh f)
		if reverseWinding then (
			format "% % %\n" (int(face[3])-1) (int(face[2])-1) (int(face[1])-1) to:out
		) else (
			format "% % %\n" (int(face[1])-1) (int(face[2])-1) (int(face[3])-1) to:out
		)
	)
	format "}\n" to:out


	format "faceVertexNormals = {\n" to:out
	for f = 1 to numFaces do (
		normals = meshop.getFaceRNormals mesh f
		if normals.count != 3 then (
			normals = #([0,0,0], [0,0,0], [0,0,0])
		)

		face = (getFace mesh f)
		
		if reverseWinding then (
			for i = 1 to 3 do normals[i] = -normals[i]
		)

		format "% % %\n" normals[1].x normals[1].z -normals[1].y to:out
		format "% % %\n" normals[2].x normals[2].z -normals[2].y to:out
		format "% % %\n" normals[3].x normals[3].z -normals[3].y to:out
	)
	format "}\n" to:out


	format "faceMaterialIds = {\n" numFaces to:out
	for f = 1 to numFaces do (
		format "%\n" (getFaceMatId mesh f) to:out
	)
	format "}\n" to:out


	numMaps = meshop.getNumMaps mesh
	format "maps = %\n" numMaps to:out
	for m = 1 to numMaps do (
		if meshop.getMapSupport mesh (m-1) then (
			format "map = {\n" to:out

			verts = meshop.getNumMapVerts mesh (m-1)
			format "verts = % {\n" verts to:out
			for v = 1 to verts do (
				vert = meshop.getMapVert mesh (m-1) v
				format "% % %\n" vert.x vert.y vert.z to:out
			)
			format "}\n" to:out

			faces = meshop.getNumMapFaces mesh (m-1)
			format "faces = % {\n" faces to:out
			for f = 1 to faces do (
				face = meshop.getMapFace mesh (m-1) f
				if reverseWinding then (
					format "% % %\n" (int(face[3])-1) (int(face[2])-1) (int(face[1])-1) to:out
				) else (
					format "% % %\n" (int(face[1])-1) (int(face[2])-1) (int(face[3])-1) to:out
				)
			)
			format "}\n" to:out

			format "}\n" to:out
		) else (
			format "map\n" to:out
		)
	) 

	format "}\n" to:out
)


fn exportHelperNode node =
(
	format "type = 'helper'\n" to:out
)


fn exportGeomNode node =
(
	format "type = 'geom'\n" to:out
	mesh = (snapshotAsMesh node)

	if mesh == undefined then (
		format "noMesh\n" to:out
	) else (
		xform = node.transform		-- world transform
		xform = (xform.rotationPart as matrix3) * (transMatrix xform.translationPart)
		local reverseWinding = false
		if isProperty node "scale" then (
			local scl = node.scale
			if scl.x < 0 do reverseWinding = not reverseWinding
			if scl.y < 0 do reverseWinding = not reverseWinding
			if scl.z < 0 do reverseWinding = not reverseWinding
		)
		xform = inverse xform
		transform mesh xform		-- bring the mesh data to node's local coordinate space
		--local objTransform = (scaleMatrix node.objectOffsetScale) * (node.objectOffsetRot as matrix3) * (transMatrix node.objectOffsetPos)
		--transform mesh objTransform
		exportMesh mesh reverseWinding
	)
)


fn exportSkyLightNode node =
(
	format "lightType = 'sky'\n" to:out

	format "enabled = %\n" node.on to:out
	format "castShadows = %\n" node.castShadows to:out
	format "rgb = % % %\n" node.color.r node.color.g node.color.b to:out
	format "multiplier = %\n" node.multiplier to:out

	if node.sky_color_map_on and node.sky_color_map != undefined then (
		format "skyColorMap = " to:out
		exportMap node.sky_color_map
		format "skyColorMapAmount = %\n" node.sky_color_map_amt to:out
	)

	modes = #("color", "environment")
	format "skyMode = '%'\n" modes[node.sky_mode + 1] to:out
	format "raysPerSample = %\n" node.rays_per_sample to:out
	format "rayBias = %\n" node.ray_bias to:out
)


fn exportLightNode node =
(
	format "type = 'light'\n" to:out

	if (classof node) == Skylight then (
		exportSkyLightNode node
	) else (
		format "lightType = '%'\n" (node.type as string) to:out

		props = #
		(
			"enabled",
			"castShadows",
			"rgb",
			"saturation",
			"multiplier",
			"contrast",
			"softenDiffuseEdge",
			"affectDiffuse",
			"affectSpecular",
			"ambientOnly",
			"nearAttenStart",
			"nearAttenEnd",
			"useNearAtten",
			"farAttenStart",
			"farAttenEnd",
			"useFarAtten",
			"decayRadius",
			"useGlobalShadowSettings",
			"shadowColor",
			"shadowMultiplier",
			"twoSidedShadows",
			"lightAffectsShadow",
			"atmosShadows",
			"atmosOpacity"
		)

		exportProps node props
		format "atmosColorAmount = %\n" node.atmosColorAmt to:out

		if node.projector and node.projectorMap != undefined then (
			format "projectorMap = " to:out
			exportMap node.projectorMap
		)

		if node.useShadowProjectorMap and node.shadowProjectorMap != undefined then (
			format "shadowProjectorMap = " to:out
			exportMap node.shadowProjectorMap 
		)



		if #omni != node.type then
		(
			props = #
			(
				"aspect",
				"falloff",
				"hotspot",
				"overShoot"
			)

			exportProps node props

			coneShapes = #("circle", "rectangle")
			format "coneShape = '%'\n" coneShapes[node.coneShape] to:out
		)
	)
)


fn exportUnsupportedNode node =
(
	format "type = 'unsupported'\n" to:out
)


fn getBoneList node =
(
	res = #()
	stack = #()

	append stack (biped.getNode (physiqueOps.getVertexBone node 1 1) #pelvis)
	while (stack.count != 0) do (
		cur = stack[stack.count]
		deleteItem stack stack.count

		append res cur

		for ch in cur.children do (
			append stack ch
		)
	)

	return res
)


fn getTimeList bone =
(
	keys = bone.controller.keys

	res = #()
	for k in keys do (
		append res k.time
	)
	
	return res
)


fn sampleBoneMotion bone timeList =
(
	if (0 == timeList.count) then (
		append timeList 0f
	)

	for t in timeList do at time t (
		format "key = % " (float(t - animationRange.start) / float(animationRange.end - animationRange.start)) to:out
		exportNodeTransform bone.transform bone.parent
	)
)


fn exportVertexWeights node bones =
(
	vcount = physiqueOps.getVertexCount node

	format " % {\n" vcount to:out

	for v=1 to vcount do (
		numBones = physiqueOps.getVertexBoneCount node v

		for b=1 to numBones do (
			bi = findItem bones (physiqueOps.getVertexBone node v b) - 1
			w = physiqueOps.getVertexWeight node v b

			format "% %  " bi w to:out
		)

		format "\n" to:out
	)

	format "}\n" to:out
)


fn exportVertexOffsets node bones =
(
	vcount = physiqueOps.getVertexCount node

	format " % {\n" vcount to:out

	for v=1 to vcount do (
		numBones = physiqueOps.getVertexBoneCount node v

		for b=1 to numBones do (
			bi = findItem bones (physiqueOps.getVertexBone node v b) - 1
			offset = physiqueOps.getVertexOffset node v b

			format "% % % %  " bi offset.x offset.z -offset.y to:out
		)

		format "\n" to:out
	)

	format "}\n" to:out
)


fn exportPhysique node =
(
	bones = getBoneList node

	format "bones =" to:out
	for b in bones do (
		format " '%'" b.name to:out
	)
	format "\n" to:out

	format "boneParents =" to:out
	for b in bones do (
		format " %" ((findItem bones b.parent)-1) to:out
	)
	format "\n" to:out

	format "weights =" to:out
	exportVertexWeights node bones

	format "offsets =" to:out
	exportVertexOffsets node bones

	for b in bones do (
		format "boneMotion = {\n" to:out
		timeList = getTimeList b
		sampleBoneMotion b timeList
		format "}\n" to:out
	)
)


fn exportNode node =
(
	curSceneObject += 1
	progressUpdate (100.0 * curSceneObject / numSceneObjects)

	format "node = {\n" to:out
	format "name = '%'\n" node.name to:out

	-- export node's transform
	format "transform = " to:out
	exportNodeTransform node.transform node.parent

	-- export a specific node type
	scl = superclassof node
	if scl == Helper then (
		exportHelperNode node
	) else if scl == GeometryClass then (
		exportGeomNode node
	) else if scl == Light then (
		exportLightNode node
	) else (
		exportUnsupportedNode node
	)

	-- export node's material
	if (false == (isProperty node "material")) or (node.material == undefined) then (
--		format "material = 'undefined'\n" to:out
	) else (
		format "material = '%'\n" node.material.name to:out
	)

	if (node.modifiers["physique"] != undefined) then (
		format "physique = {\n" to:out
		exportPhysique node
		format "}\n" to:out
	)


	if (isProperty node "children") then (
		-- export node's children count
		format "children = %\n" node.children.count to:out

		-- export node's children
		for ch in node.children do (
			exportNode ch
		)
	) else (
		format "children = 0\n" to:out
	)

	format "}\n" to:out
)


fn exportBitmap map =
(
	texPath = ""
	if BitmapTexture == (classof map) then (
		texPath = map.filename
	) else (
		texPath = "textures\\" + map.name + ".png"
		rm = renderMap map size:renderTextureSize fileName:(dumpPath + texPath)
		save rm
		close rm
	)
	format "bitmap = '%'\n" texPath to:out
)


fn exportMap map =
(
	format "{\n" to:out

	cl = classof map

	if Reflect_Refract == cl then (
		format "type = 'reflection'\n" to:out
	) else if Flat_Mirror == cl then (
		format "type = 'flatMirror'\n" to:out
	) else if Raytrace == cl then (
		format "type = 'raytrace'\n" to:out
	) else (
		format "type = 'bitmap'\n" to:out
		exportBitmap map
	)

	format "}\n" to:out
)


fn exportMaterialMap mat map =
(
	local mapEnable = map + "MapEnable"
	local realMap = undefined
	local enabled = false
	
	if (isProperty mat (map+"Map")) and (isProperty mat mapEnable) then (
		realMap = getProperty mat (map+"Map")
		enabled = getProperty mat mapEnable
	)
	
	if enabled then (
		format (map + "Map = ") to:out
		exportMap realMap

		amount = map + "MapAmount"

		if isProperty mat amount then (
			amnt = (getProperty mat amount)
		) else (
			amnt = 100.0
		)

		format (map + "MapAmount = %\n") amnt to:out
	)
)


fn exportUnsupportedMaterial mat =
(
	format "type = 'unsupported'\n" to:out
)


fn exportNoMaterial mat =
(
	format "type = 'no'\n" to:out
)


fn exportArchMaterial mat =
(
	format "type = 'architectural'\n" to:out
	
	props = #(
		"colorBleed",
		"diffuse",
		"emitLuminance",
		"indirectBumpAmount",
		"luminance",
		"rawDiffuseTexture",
		"reflectanceScale",
		"shininess",
		"template",
		"textureUOffset",
		"textureVOffset",
		"translucency",
		"transparency",
		"twoSided"
	)

	exportProps mat	props

	exportMaterialMap mat "diffuse"
	exportMaterialMap mat "bump"
	exportMaterialMap mat "cutout"
	exportMaterialMap mat "displacement"
	exportMaterialMap mat "intensity"
	exportMaterialMap mat "luminance"
	exportMaterialMap mat "shininess"
	exportMaterialMap mat "translucency"
	exportMaterialMap mat "transparency"
)


fn exportMultiMaterial mat =
(
	format "type = 'multi'\n" to:out
	format "numSubs = %\n" mat.numSubs to:out
	format "subIds = " to:out
	for i in mat.materialIDList do (
		format " %" i to:out
	)
	format "\n" to:out
	for m in mat.materialList do (
		exportMaterial m
	)
)


fn exportBlendMaterial mat =
(
	format "type = 'blend'\n" to:out
	
	props = #(
		"map1Enabled",
		"map2Enabled",
		"maskEnabled",
		"mixAmount",
		"useCurve",
		"upper",
		"lower"
	)

	exportProps mat	props

	if mat.map1Enabled then (
		exportMaterial mat.map1
	)
	if mat.map2Enabled then (
		exportMaterial mat.map2
	)
	if mat.maskEnabled then (
		exportMaterial mat.mask
	)
)


fn exportStandardMaterial2 mat =
(
	props = #(
		"ambient",
		"diffuse",
		"specular",
		"specularLevel",
		"glossiness",
		"selfIllumAmount",
		"selfIllumColor",
		"opacity"
	)

	exportProps mat	props

	exportMaterialMap mat "diffuse"
	exportMaterialMap mat "ambient"
	exportMaterialMap mat "specular"
	exportMaterialMap mat "glossiness"
	exportMaterialMap mat "selfIllum"
	exportMaterialMap mat "opacity"
	exportMaterialMap mat "filter"
	exportMaterialMap mat "bump"
	exportMaterialMap mat "reflection"
	exportMaterialMap mat "refraction"
	exportMaterialMap mat "displacement"
)


fn exportMultiLayerMaterial2 mat =
(
	props = #(
		"ambient",
		"diffuse",
		"specular",
		"glossiness",
		"specularLevel",
		"selfIllumAmount",
		"selfIllumColor",
		"anisotropy",
		"orientation",
		"opacity",
		"specular2",
		"specularLevel2",
		"glossiness2",
		"anisotropy2",
		"orientation2"
	)

	exportProps mat	props
)


fn exportStandardMaterial mat =
(
	format "type = 'standard'\n" to:out
	props = #(
		"shaderByName",
		"wire",
		"twoSided",
		"faceted",
		"opacity",
		"filterColor",
		"wireSize",
		"applyReflectionDimming",
		"dimLevel",
		"reflectionLevel"
	)

	exportProps mat props

	opacityTypes = #("filter", "subtract", "add")
	format "opacityType = '%'\n" opacityTypes[mat.opacityType + 1] to:out

	if mat.shaderByName != "Multi-Layer" then (
		exportStandardMaterial2 mat
	) else (
		exportMultiLayerMaterial mat
	)
)


fn exportMaterial mat =
(
	format "material = {\n" to:out
	format "name = '%'\n" mat.name to:out

	cl = classof mat

	if cl == Multimaterial then (
		exportMultiMaterial mat
	) else if cl == NoMaterial then (
		exportNoMaterial mat
	) else if cl == StandardMaterial then (
		exportStandardMaterial mat
	) else if cl == Blend then (
		exportBlendMaterial mat
	) else if cl == ArchitecturalMaterial then (
		exportArchMaterial mat
	) else (
		exportUnsupportedMaterial mat
	)

	format "}\n" to:out
)


fn exportMaterials =
(
	progressStart "Exporting materials"

	total	= sceneMaterials.count
	cur		= 0

	format "materials = %\n" total to:out
	for mat in sceneMaterials do (
		exportMaterial mat

		cur += 1
		progressUpdate (100.0 * cur / total)
	)

	progressEnd()
)


fn countSceneObjects node =
(
	numSceneObjects += 1

	if (isProperty node "children") then (
		for ch in node.children do (
			countSceneObjects ch
		)
	)
)


fn exportGlobals =
(
	format "globals = {\n" to:out

	format "exporterInfo = 'Heresy Max Exporter 0.2'\n" to:out
	format "ambientColor = % % %\n" ambientColor.r ambientColor.g ambientColor.b to:out
	format "backgroundColor = % % %\n" backgroundColor.r backgroundColor.g backgroundColor.b to:out
	format "displayGamma = %\n" displayGamma to:out
	format "lightTintColor = % % %\n" lightTintColor.r lightTintColor.g lightTintColor.b to:out
	format "lightLevel = %\n" lightLevel to:out
	
	if useEnvironmentMap and environmentMap != undefined then (
		format "environmentMap = " to:out
		exportMap environmentMap
	)

	format "}\n" to:out
)


fn exportSceneObjects =
(
	numSceneObjects	= 0
	curSceneObject	= 0

	progressStart "Exporting scene objects"

	for n in rootNode.children do (
		countSceneObjects n
	)

	format "totalNodes = %\n" numSceneObjects to:out
	format "nodes = %\n" rootNode.children.count to:out

	for n in rootNode.children do (
		exportNode n
	)

	progressEnd()
)


fn main =
(
	sceneFile = "scene.hme"
	sceneFile = dumpPath + sceneFile

	out = createFile sceneFile

	makeDir (dumpPath + "textures")

	exportGlobals()
	exportMaterials()
	exportSceneObjects()

	close out
	out = undefined
)


utility HeresyExporter "Heresy Exporter"
(
	dropdownlist texSize "Render texture size" items:
		#("32", "64", "128", "256", "512", "1024", "2048", "4096") selection:5

	button export "Export" width:150

	on export pressed do (
		dumpPath = getSavePath()
		if dumpPath != undefined then (
			if dumpPath[dumpPath.count] != "\\" then (
				dumpPath = dumpPath + "\\"
			)

			s = int(texSize.selected as float)
			renderTextureSize = [s,s]

			undo off (
				main()
			)
		)
	)
)
