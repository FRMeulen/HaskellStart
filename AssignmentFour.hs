module AssignmentFour
    where
    data Color = Red | Blue | Yellow | Green
        deriving Show
    data Geofig = Square Float Color | Rectangle Float Float Color | Triangle Float Color | Circle Float Color
        deriving Show

    --Geofigs for testing
    redSquare = Square 5 Red
    blueRectangle = Rectangle 2 4 Blue
    yellowTriangle = Triangle 7 Yellow
    greenCircle = Circle 8 Green
    yellowRectangle = Rectangle 2 8 Yellow
    blueCircle = Circle 3 Blue
    greenSquare = Square 6 Green
    redTriangle = Triangle 1 Red

    --List of Geofigs for testing
    shapesList = [redSquare, blueRectangle, yellowTriangle, greenCircle, yellowRectangle, blueCircle, greenSquare, redTriangle]
    surfaceList = getSurfaces shapesList

    surface::Geofig -> Float    --Returns surface of Geofig
    surface (Square length _) = length^2
    surface (Rectangle length width _) = length*width
    surface (Triangle length _) = 0.25 * length^2 * sqrt(3)
    surface (Circle radius _) = pi * radius^2

    circumference::Geofig -> Float  --Returns circumference of Geofig
    circumference (Square length _) = length*4
    circumference (Rectangle length width _) = 2*length + 2*width
    circumference (Triangle length _) = 3*length
    circumference (Circle radius _) = 2*pi * radius

    isSquare::Geofig -> Bool    --True if Geofig is Square
    isSquare (Square _ _) = True
    isSquare _ = False

    isRectangle::Geofig -> Bool --True if Geofig is Rectangle
    isRectangle (Rectangle _ _ _) = True
    isRectangle _ = False

    isTriangle::Geofig -> Bool  --True if Geofig is Triangle
    isTriangle (Triangle _ _) = True
    isTriangle _ = False

    isCircle::Geofig -> Bool    --True if Geofig is Circle
    isCircle (Circle _ _) = True
    isCircle _ = False

    getSquares::[Geofig] -> [Geofig]    --Filters Squares from list of Geofigs
    getSquares list = filter(isSquare) list

    getRectangles::[Geofig] -> [Geofig] --Filters Rectangles from list of Geofigs
    getRectangles list = filter(isRectangle) list

    getTriangles::[Geofig] -> [Geofig]  --Filters Triangles from list of Geofigs
    getTriangles list = filter(isTriangle) list

    getCircles::[Geofig] -> [Geofig]    --Filters Circles from list of Geofigs
    getCircles list = filter(isCircle) list

    getShapes::String -> [Geofig] -> [Geofig]   --General filter function
    getShapes "square" list = filter(isSquare) list
    getShapes "rectangle" list = filter(isRectangle) list
    getShapes "triangle" list = filter(isTriangle) list
    getShapes "circle" list = filter(isCircle) list

    isRed::Geofig -> Bool   --True if Geofig is Red
    isRed (Square _ Red) = True
    isRed (Rectangle _ _ Red) = True
    isRed (Triangle _ Red) = True
    isRed (Circle _ Red) = True
    isRed _ = False

    isBlue::Geofig -> Bool  --True if Geofig is Blue
    isBlue (Square _ Blue) = True
    isBlue (Rectangle _ _ Blue) = True
    isBlue (Triangle _ Blue) = True
    isBlue (Circle _ Blue) = True
    isBlue _ = False

    isYellow::Geofig -> Bool    --True if Geofig is Yellow
    isYellow (Square _ Yellow) = True
    isYellow (Rectangle _ _ Yellow) = True
    isYellow (Triangle _ Yellow) = True
    isYellow (Circle _ Yellow) = True
    isYellow _ = False
    
    isGreen::Geofig -> Bool --True if Geofig is Green
    isGreen (Square _ Green) = True
    isGreen (Rectangle _ _ Green) = True
    isGreen (Triangle _ Green) = True
    isGreen (Circle _ Green) = True
    isGreen _ = False

    getAllOfColor::Color -> [Geofig] -> [Geofig]    --Filters list of Geofigs by Color
    getAllOfColor Red list = filter(isRed) list
    getAllOfColor Blue list = filter(isBlue) list
    getAllOfColor Yellow list = filter(isYellow) list
    getAllOfColor Green list = filter(isGreen) list

    getSurfaces::[Geofig] -> [Float]    --Returns list of Geofig surfaces
    getSurfaces list = map surface(list)

    getSurfaceTuple::Geofig -> (Geofig, Float)
    getSurfaceTuple a = (a, surface a)

    getSurfaceTuples::[Geofig] -> [(Geofig, Float)] --Returns tuples of Geofig with surface
    getSurfaceTuples list = map getSurfaceTuple list

    getCircumferenceTuple::Geofig -> (Geofig, Float)
    getCircumferenceTuple a = (a, circumference a)

    getCircumferenceTuples::[Geofig] -> [(Geofig, Float)]   --Returns tuples of Geofig with circumference
    getCircumferenceTuples list = map getCircumferenceTuple list

    toTuple::[a] -> (a) --Converts list to tuple
    toTuple [a] = (a)

    getBiggestSurface::[Geofig] -> (Geofig, Float)  --Returns Geofig with biggest surface in tuple
    getBiggestSurface list = toTuple maxSurfaceList
        where 
            maxSurfaceList = filter (elem maxSurface) tuples
            tuples = getSurfaceTuples list
            maxSurface = maximum surfaces
            surfaces = map snd tuples

    getBiggestCircumference::[Geofig] -> (Geofig, Float)    --Returns Geofig with biggest circumference in tuple
    getBiggestCircumference list = toTuple maxCircumferenceList
        where 
            maxCircumferenceList = filter (elem maxCircumference) tuples
            tuples = getCircumferenceTuples list
            maxCircumference = maximum circumferences
            circumferences = map snd tuples

    addToShapesList::Geofig -> [Geofig] --Adds Geofig to list of Geofigs
    addToShapesList shape = shapesList ++ [shape]

    getTotalSurface::[Geofig] -> Float  --Returns total surface of all Geofigs in list
    getTotalSurface list = sum $map surface list

    getSurfacePercentages::[Geofig] -> [Float]  --Returns list of surface percentages
    getSurfacePercentages list = map (* 100) $map (/ getTotalSurface list) $map surface list