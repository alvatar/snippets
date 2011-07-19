#!BPY
"""
Name: 'Solidify selected edges (wire)'
Blender: 234
Group: 'Mesh'
Tooltip: 'make tubes from selected edges'
"""

__author__ = "Campbell Barton AKA Ideasman"
__url__ = ["http://members.iinet.net.au/~cpbarton/ideasman/", "blender", "elysiun"]

from Blender import *
from Blender.Mathutils import TriangleNormal, Vector


#=============================#
# Blender functions/shortcuts #
#=============================#
def error(str):
	Draw.PupMenu('ERROR%t|'+str)


def measure(v1, v2):
  return Mathutils.Vector([v1[0]-v2[0], v1[1] - v2[1], v1[2] - v2[2]]).length


def makeVNormal(me, vert, origEdgeLen):
	vNormal = Vector([0,0,0])
	eIdx = 0
	while eIdx < origEdgeLen:
		
		e = me.edges[eIdx]
		
		if e.v1 == vert or e.v2 == vert:
			pass
		else:
			eIdx+=1
			continue
			
		v1 = -1
		if vert == e.v1: v1 = False
		if vert == e.v2: v1 = 1
		if v1 == -1:
			eIdx+=1
			continue
			
		# one of the verts in the face is the same as the vert that we are finding teyh normal for.
		# Make a vector from the edge v1 and v2 are the indivies for this edge
		v2 = (not v1)
		
		ev = [e.v1, e.v2]
		
		newVec = Vector([\
			ev[v1].co[0]-ev[v2].co[0],\
			ev[v1].co[1]-ev[v2].co[1],\
			ev[v1].co[2]-ev[v2].co[2]] )
		
		newVec.normalize()
		vNormal = vNormal + newVec
		eIdx+=1
		
	vNormal.normalize()
	return vNormal


def wireMesh(me):
	globalSize = Draw.PupFloatInput('wire width', 0.1, -10.0, 10.0, 0.01, 4)
	if globalSize == None:
		return
	globalHalfsz =globalSize/2
	
	ratio = Draw.PupMenu('Edge Width%t|Fixed Width|Scale with edge len')
	
	normalMethod = Draw.PupMenu('Use Normal%t|Vertex Normal|Edge Normal')
	
	if ratio == -1 or normalMethod == -1:
		return
	
	t = sys.time()
	origEdgeLen = len(me.edges)
	origFaceLen = len(me.faces)
	eIdx = 0
	while eIdx < origEdgeLen:
		
		e = me.edges[eIdx]
		if not e.flag & NMesh.EdgeFlags['SELECT']:
			eIdx+=1
			continue
		
		
		if ratio == 1:
			size = globalSize
			halfsz = globalHalfsz
		else:
			size = globalSize * measure(e.v1, e.v2)
			halfsz = size/2
		
		
		if normalMethod == 1: # vertex normal
			v1nor = makeVNormal(me, e.v1, origEdgeLen)
			v2nor = makeVNormal(me, e.v2, origEdgeLen)
			
			newPt1 = Vector( e.v1.co[0] + (v1nor[0]*size), e.v1.co[1] + (v1nor[1]*size), e.v1.co[2] + (v1nor[2]*size) )
			newPt2 = Vector( e.v2.co[0] + (v2nor[0]*size), e.v2.co[1] + (v2nor[1]*size), e.v2.co[2] + (v2nor[2]*size) )
			
		if normalMethod == 2: # Face Normal
			edgeNormal = Vector([e.v1.no[0] + e.v2.no[0], e.v1.no[1]+e.v2.no[1], e.v1.no[2]+e.v2.no[2] ])
			edgeNormal.normalize()
			
			newPt1 = Vector( e.v1.co[0] + (edgeNormal[0]*size), e.v1.co[1] + (edgeNormal[1]*size), e.v1.co[2] + (edgeNormal[2]*size) )
			newPt2 = Vector( e.v2.co[0] + (edgeNormal[0]*size), e.v2.co[1] + (edgeNormal[1]*size), e.v2.co[2] + (edgeNormal[2]*size) )			
		
		#		#~ newPt1************newPt2
		#		#~ *                      *
		#		#~ *                      *
		#		#~ *                      *
		#	#~ ****vert1**************Vert2**** * * * *
		norA = TriangleNormal(e.v1.co, e.v2.co, newPt1)
		norB = TriangleNormal(e.v2.co, newPt2, newPt1)
		nor1 = norA + norB
		nor1.normalize()
		
		# make face A
		me.verts.append(NMesh.Vert(e.v1.co[0] + (nor1[0]*halfsz), e.v1.co[1] + (nor1[1]*halfsz), e.v1.co[2] + (nor1[2]*halfsz)) )
		me.verts.append(NMesh.Vert(e.v2.co[0] + (nor1[0]*halfsz), e.v2.co[1] + (nor1[1]*halfsz), e.v2.co[2] + (nor1[2]*halfsz)) )
		
		me.verts.append(NMesh.Vert(newPt2[0] + (nor1[0]*halfsz), newPt2[1] + (nor1[1]*halfsz), newPt2[2] + (nor1[2]*halfsz)) )
		me.verts.append(NMesh.Vert(newPt1[0] + (nor1[0]*halfsz), newPt1[1] + (nor1[1]*halfsz), newPt1[2] + (nor1[2]*halfsz)) )
		
		fA = NMesh.Face(me.verts[-4:])		
		
		# make face B
		me.verts.append(NMesh.Vert(e.v1.co[0] + (nor1[0]*-halfsz), e.v1.co[1] + (nor1[1]*-halfsz), e.v1.co[2] + (nor1[2]*-halfsz)) )
		me.verts.append(NMesh.Vert(e.v2.co[0] + (nor1[0]*-halfsz), e.v2.co[1] + (nor1[1]*-halfsz), e.v2.co[2] + (nor1[2]*-halfsz)) )
		
		me.verts.append(NMesh.Vert(newPt2[0] + (nor1[0]*-halfsz), newPt2[1] + (nor1[1]*-halfsz), newPt2[2] + (nor1[2]*-halfsz)) )
		me.verts.append(NMesh.Vert(newPt1[0] + (nor1[0]*-halfsz), newPt1[1] + (nor1[1]*-halfsz), newPt1[2] + (nor1[2]*-halfsz)) )
		
		fB = NMesh.Face([me.verts[-1], me.verts[-2], me.verts[-3], me.verts[-4]])
		
		# make face C- top
		fC = NMesh.Face([fB.v[1], fB.v[0], fA.v[3], fA.v[2]])
		
		# make face D- bottom
		fD = NMesh.Face([fA.v[1], fA.v[0], fB.v[3], fB.v[2]])
		
		
		# a negative number is used for an inset wire- this flips the normals the wrong way- For easyness this is a simple way to fix the problem.
		if globalSize < 0:
			fA.v.reverse()
			fB.v.reverse()
			fC.v.reverse()
			fD.v.reverse()
		
		me.faces.extend([fA, fB, fC, fD])
		eIdx+=1
	print 'Wire Time: %.6f' % (sys.time()-t)
	
is_editmode = Window.EditMode()
if is_editmode: Window.EditMode(0)

# Get a mesh and raise errors if we cant
mesh = None
if len(Object.GetSelected()) > 0:
	object = Object.GetSelected()[0]
	if object.getType() == 'Mesh':
		mesh = Object.GetSelected()[0].getData()
	else:
		error('please select a mesh')
else:
	error('no mesh object selected')			

if mesh != None:
	print 'sd'
	# The Main function
	
	wireMesh(mesh)
	
	mesh.update()

if is_editmode:
	Window.EditMode(1)

