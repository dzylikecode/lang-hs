class Shape {};

class Circle: public Shape {
public:
    Circle(float x, float y, float r): x(x), y(y), r(r) {}
    float x, y, r;
};

class Rectangle: public Shape {
public:
    Rectangle(float x1, float y1, float x2, float y2): x1(x1), y1(y1), x2(x2), y2(y2) {}
    float x1, y1, x2, y2;
};

float surface(const Circle&  c) { return 3.14 * c.r * c.r; }
float surface(const Rectangle& r) { return (r.x2 - r.x1) * (r.y2 - r.y1); }

#include <iostream>

int main() {
    Circle c(0, 0, 1);
    Rectangle r(0, 0, 1, 1);
    std::cout << "surface(Circle):" << surface(c) << std::endl;
    std::cout << "surface(Rectangle):" << surface(r) << std::endl;
    // Shape *s = &c;
    // std::cout << "surface(Shape):" << surface(*s) << std::endl;
    // c++ 会报错
    return 0;
}