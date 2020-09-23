xof 0302txt 0064

template Header {
 <3D82AB43-62DA-11CF-AB39-0020AF71E433>
 WORD major;
 WORD minor;
 DWORD flags;
}

template Coords2d {
 <F6F23F44-7686-11CF-8F52-0040333594A3>
 FLOAT u;
 FLOAT v;
}

template Vector {
 <3D82AB5E-62DA-11CF-AB39-0020AF71E433>
 FLOAT x;
 FLOAT y;
 FLOAT z;
}

template ColorRGB {
 <D3E16E81-7835-11CF-8F52-0040333594A3>
 FLOAT red;
 FLOAT green;
 FLOAT blue;
}

template ColorRGBA {
 <35FF44E0-6C7C-11CF-8F52-0040333594A3>
 FLOAT red;
 FLOAT green;
 FLOAT blue;
 FLOAT alpha;
}

template Material {
 <3D82AB4D-62DA-11CF-AB39-0020AF71E433>
 ColorRGBA faceColor;
 FLOAT power;
 ColorRGB specularColor;
 ColorRGB emissiveColor;
 [...]
}

template MeshFace {
 <3D82AB5F-62DA-11CF-AB39-0020AF71E433>
 DWORD nFaceVertexIndices;
 array DWORD faceVertexIndices[nFaceVertexIndices];
}

template MeshMaterialList {
 <F6F23F42-7686-11CF-8F52-0040333594A3>
 DWORD nMaterials;
 DWORD nFaceIndexes;
 array DWORD faceIndexes[nFaceIndexes];
 [Material]
}

template MeshNormals {
 <F6F23F43-7686-11CF-8F52-0040333594A3>
 DWORD nNormals;
 array Vector normals[nNormals];
 DWORD nFaceNormals;
 array MeshFace faceNormals[nFaceNormals];
}

template MeshTextureCoords {
 <F6F23F40-7686-11CF-8F52-0040333594A3>
 DWORD nTextureCoords;
 array Coords2d textureCoords[nTextureCoords];
}

template Mesh {
 <3D82AB44-62DA-11CF-AB39-0020AF71E433>
 DWORD nVertices;
 array Vector vertices[nVertices];
 DWORD nFaces;
 array MeshFace faces[nFaces];
 [...]
}

Header {
 1;
 0;
 1;
}

Mesh {
 6;
 0.000000;0.000000;50.000000;,
 -0.000002;50.000000;-0.000002;,
 -50.000000;-0.000004;-0.000002;,
 0.000007;-50.000000;-0.000002;,
 50.000000;0.000009;-0.000002;,
 0.000000;0.000000;-50.000000;;

 8;
 3;0,1,2;,
 3;0,2,3;,
 3;0,3,4;,
 3;0,4,1;,
 3;5,2,1;,
 3;5,3,2;,
 3;5,4,3;,
 3;5,1,4;;

 MeshMaterialList {
  1;
  1;
  0;;
  Material {
   1.000000,1.000000,1.000000,1.000000;;
   0.000000;
   0.900000,0.900000,0.900000;;
   0.000000,0.000000,0.000000;;
  }
 }

 MeshNormals {
  6;
  0.000000;0.000000;1.000000;,
  -0.000000;1.000000;-0.000000;,
  -1.000000;-0.000000;-0.000000;,
  0.000000;-1.000000;0.000000;,
  1.000000;0.000000;-0.000000;,
  0.000000;0.000000;-1.000000;;

  8;
  3;0,1,2;,
  3;0,2,3;,
  3;0,3,4;,
  3;0,4,1;,
  3;5,2,1;,
  3;5,3,2;,
  3;5,4,3;,
  3;5,1,4;;
 }

 MeshTextureCoords {
  15;
  0.000000;1.000000;,
  0.250000;1.000000;,
  0.500000;1.000000;,
  0.750000;1.000000;,
  1.000000;1.000000;,
  0.000000;0.500000;,
  0.250000;0.500000;,
  0.500000;0.500000;,
  0.750000;0.500000;,
  1.000000;0.500000;,
  0.000000;0.000000;,
  0.250000;0.000000;,
  0.500000;0.000000;,
  0.750000;0.000000;,
  1.000000;0.000000;;
 }
}
