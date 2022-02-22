width = 1024
height = 768
xMin = -2.25
xMax = 1.125
yMin = -1.125
yMax = 1.125

getSlope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1) 
getYIntercept (x1, y1) (x2, y2) = y1 - (slope * x1)
  where slope = getSlope (x1, y1) (x2, y2)

-- [0, 1024] —> [-1, 1]
mapRange (a1, a2) (b1, b2) x = slope * x + yIntercept
  where slope = getSlope (a1, b1) (a2, b2)
        yIntercept = getYIntercept (a1, b1) (a2, b2)

mapPixelToReal (n, m) = (mapX n, mapY m) 
  where mapX = mapRange (0, width) (xMin, xMax)
        mapY = mapRange (0, height) (yMax, yMin)

mapRealToPixel (n, m) = (mapX n, mapY m) 
  where mapX = mapRange (xMin, xMax) (0, width)
        mapY = mapRange (yMax, yMin) (0, height)
