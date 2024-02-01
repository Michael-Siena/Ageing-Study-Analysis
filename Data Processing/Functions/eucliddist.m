function distance = EuclideanDistance(x0, y0, x1, y1)
    dX = x1 - x0;
    dY = y1 - y0;
    
    distance = sqrt((dX * dX) + (dY * dY));
end

