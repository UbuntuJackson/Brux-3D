donut("utils.nut")
donut("objects.nut")

setFPS(60)
setResolution(1200, 960)
setWindowTitle("3D")
::gvQuit <- false

setDrawColor(0xFFFFFFFF)

// Projection Matrix
::fNear <- 0.1;
::fFar <- 1000.0;
::fFov <- 90.0;
::fAspectRatio <- screenH().tofloat() / screenW().tofloat();
::fFovRad <- 1.0 / tan(fFov * 0.5 / 180.0 * 3.14159);
::matProj <- [
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0]
]
matProj[0][0] = fAspectRatio * fFovRad;
matProj[1][1] = fFovRad;
matProj[2][2] = fFar / (fFar - fNear);
matProj[3][2] = (-fFar * fNear) / (fFar - fNear);
matProj[2][3] = 1.0;
matProj[3][3] = 0.0;

//Rotation Matrix

::fTheta <- 0

::matRotZ <- [
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0]
]
::matRotX <- [
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 0.0, 0.0]
]

::triProjected <- [{x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}]
::triTranslated <- [{x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}]
::triRotatedZ <- [{x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}]
::triRotatedZX <- [{x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}]


::multiplyVector <- function(_i, _o, _m)
{
    _o.x = _i.x * _m[0][0] + _i.y * _m[1][0] + _i.z * _m[2][0] + _m[3][0];
	_o.y = _i.x * _m[0][1] + _i.y * _m[1][1] + _i.z * _m[2][1] + _m[3][1];
	_o.z = _i.x * _m[0][2] + _i.y * _m[1][2] + _i.z * _m[2][2] + _m[3][2];
	local w = _i.x * _m[0][3] + _i.y * _m[1][3] + _i.z * _m[2][3] + _m[3][3];

	if(w != 0.0)
	{
		_o.x /= w; _o.y /= w; _o.z /= w;
	}
}

::t <- ThreeDeeObject(
    [
        //SOUTH
        [
            {x = 0.0, y = 0.0, z = 0.0}, {x = 0.0, y = 1.0, z = 0.0}, {x = 1.0, y = 1.0, z = 0.0}
        ],
        [
            {x = 0.0, y = 0.0, z = 0.0}, {x = 1.0, y = 1.0, z = 0.0}, {x = 1.0, y = 0.0, z = 0.0}
        ],

        //EAST
        [
            {x = 1.0, y = 0.0, z = 0.0}, {x = 1.0, y = 1.0, z = 0.0}, {x = 1.0, y = 1.0, z = 1.0}
        ],
        [
            {x = 1.0, y = 0.0, z = 0.0}, {x = 1.0, y = 1.0, z = 1.0}, {x = 1.0, y = 0.0, z = 1.0}
        ],

        //NORTH
        [
            {x = 1.0, y = 0.0, z = 1.0}, {x = 1.0, y = 1.0, z = 1.0}, {x = 0.0, y = 1.0, z = 1.0}
        ],
        [
		    {x = 1.0, y = 0.0, z = 1.0}, {x = 0.0, y = 1.0, z = 1.0}, {x = 0.0, y = 0.0, z = 1.0}
        ],

        //WEST
        [
            {x = 0.0, y = 0.0, z = 1.0}, {x = 0.0, y = 1.0, z = 1.0}, {x = 0.0, y = 1.0, z = 0.0}
        ],
        [
            {x = 0.0, y = 0.0, z = 1.0}, {x = 0.0, y = 1.0, z = 0.0}, {x = 0.0, y = 0.0, z = 0.0}
        ],

        //TOP
        [
            {x = 0.0, y = 1.0, z = 0.0}, {x = 0.0, y = 1.0, z = 1.0}, {x = 1.0, y = 1.0, z = 1.0}
        ],
        [
            {x = 0.0, y = 1.0, z = 0.0}, {x = 1.0, y = 1.0, z = 1.0}, {x = 1.0, y = 1.0, z = 0.0}
        ],

        //BOTTOM
        [
            {x = 1.0, y = 0.0, z = 1.0}, {x = 0.0, y = 0.0, z = 1.0}, {x = 0.0, y = 0.0, z = 0.0}
        ],
        [
            {x = 1.0, y = 0.0, z = 1.0}, {x = 0.0, y = 0.0, z = 0.0}, {x = 1.0, y = 0.0, z = 0.0}
        ]
    ]
)

while(!gvQuit && !getQuit()) {
    fTheta = getFrames() * 0.01
    // Rotation Z
    matRotZ[0][0] = cos(fTheta);
    matRotZ[0][1] = sin(fTheta);
    matRotZ[1][0] = -sin(fTheta);
    matRotZ[1][1] = cos(fTheta);
    matRotZ[2][2] = 1;
    matRotZ[3][3] = 1;

    // Rotation X
	matRotX[0][0] = 1;
	matRotX[1][1] = cos(fTheta * 0.5);
	matRotX[1][2] = sin(fTheta * 0.5);
	matRotX[2][1] = -sin(fTheta * 0.5);
    matRotX[2][2] = cos(fTheta * 0.5);
	matRotX[3][3] = 1;

    foreach(a, tri in t.tris)
    {

        multiplyVector(tri[0], triRotatedZ[0], matRotZ);
		multiplyVector(tri[1], triRotatedZ[1], matRotZ);
		multiplyVector(tri[2], triRotatedZ[2], matRotZ);

        multiplyVector(triRotatedZ[0], triRotatedZX[0], matRotX);
		multiplyVector(triRotatedZ[1], triRotatedZX[1], matRotX);
		multiplyVector(triRotatedZ[2], triRotatedZX[2], matRotX);

        triTranslated[0].x = triRotatedZX[0].x
        triTranslated[0].y = triRotatedZX[0].y
        triTranslated[0].z = triRotatedZX[0].z
        triTranslated[1].x = triRotatedZX[1].x
        triTranslated[1].y = triRotatedZX[1].y
        triTranslated[1].z = triRotatedZX[1].z
        triTranslated[2].x = triRotatedZX[2].x
        triTranslated[2].y = triRotatedZX[2].y
        triTranslated[2].z = triRotatedZX[2].z


        triTranslated[0].z = triRotatedZX[0].z + 3.0;
        triTranslated[1].z = triRotatedZX[1].z + 3.0;
        triTranslated[2].z = triRotatedZX[2].z + 3.0;

        multiplyVector(triTranslated[0], triProjected[0], matProj)
        multiplyVector(triTranslated[1], triProjected[1], matProj)
        multiplyVector(triTranslated[2], triProjected[2], matProj)

        triProjected[0].x += 1.0; triProjected[0].y += 1.0;
		triProjected[1].x += 1.0; triProjected[1].y += 1.0;
		triProjected[2].x += 1.0; triProjected[2].y += 1.0;
		triProjected[0].x *= 0.5 * screenW();
		triProjected[0].y *= 0.5 * screenH();
		triProjected[1].x *= 0.5 * screenW();
		triProjected[1].y *= 0.5 * screenH();
		triProjected[2].x *= 0.5 * screenW();
		triProjected[2].y *= 0.5 * screenH();

        drawTriangle(triProjected[0].x, triProjected[0].y, triProjected[1].x, triProjected[1].y, triProjected[2].x, triProjected[2].y)
    }

	update();
}