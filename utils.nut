::drawTriangle <- function(_x1, _y1, _x2, _y2, _x3, _y3)
{
    drawLine(_x1, _y1, _x2, _y2)
    drawLine(_x2, _y2, _x3, _y3)
    drawLine(_x3, _y3, _x1, _y1)
}