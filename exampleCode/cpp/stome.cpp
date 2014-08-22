// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      stome.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2013 by Mitch Richling.  All rights reserved.
   @brief     Well behaved class for use with STL containers and algorithms.@EOL
   @Keywords  STL
   @Std       C++11

   Notes:

     (1) Copy constructor

     (2) The move constructor (new in C++11).  NOTES: Essential for good performance of some algorithms like std::sort, and object
         must still support assignment (both move and traditional) after any 'move' operation.

     (3) The default, no-arg, constructor is required for arrays  & preallocated vectors -- make it efficient!

     (4) Destructor.  Required for STL containers, and must not throw exceptions.

     (5) Assignment operator.  Required for STL containers, and must deal well with self assignment.

     (6) The move assignment (new in C++11).  See notes item 2 & 5.

     (7) Less Than operator used for things like std::sort and std::map, std::lower_bound

     (8) == operator used for things like std::find

     (9) The 'setter/seter method' is a common idiom -- perhaps more popular in Java than C++.

    (10) This is a common idiom.  i.e. a constructor that sets everything coupled with a 'set' method taking the same arguments.

    (11) Standard for container-like things allowing lvalue access to a member, but frequently considered evil.

    (12) Not really a part of IVec2, but a common thing to do so that objects may be 'printed'.  Note that this is a more common
         idiom in C++ than item 14 below.

    (13) Construct with C++11 list

    (14) A 'to_s' member is not required, and the name is not standardized ('toString' is another common name).

***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <iomanip>
#include <locale>
#include <sstream>
#include <string>
#include <algorithm>
#include <vector>
#include <initializer_list>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DOMOVEO
#define DOMOVEC
//#define DOBAKLESS

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int constructSerial = 0;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class IVec2 {
private:
    int* vecData;
    int serial;
public:
    IVec2(std::initializer_list<int> lst);   // Construct with C++11 list (13)
    IVec2();                                 // Default constructor (3)
    IVec2(IVec2 const& iv);                  // Copy constructor (1)
#ifdef DOMOVEC                               
    IVec2(IVec2&& iv);                       // Move constructor C++11 (2)
#endif                                       
                                             
    IVec2(int x, int y);                     // Extra constructor (10)
    ~IVec2();                                // destructor (4)
                                             
    IVec2& operator= (IVec2 const& iv);      // Assignment operator (5)
#ifdef DOMOVEO                               
    IVec2& operator= (IVec2&& iv);           // Move Assignment operator C++11 (6)
#endif

    bool operator==(IVec2 const& iv) const;  // equality operator (8)
    bool operator<(IVec2 const& iv) const;   // Less Than operator (7)

    void set(int xi, int yi);                // setter method (9)
    int& setX(int newX);                     // setter method (9)
    int& setY(int newY);                     // setter method (9)
    int getX() const;                        // getter method (9)
    int getY() const;                        // getter method (9)
    int& first();                            // setf-able access to first element (11)
    int& second();                           // setf-able access to first element (11)
    std::string to_s();                      // Serialize (14)
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
IVec2::IVec2(std::initializer_list<int> lst) {
  serial = ++constructSerial;
  std::cout << "LIST CONSTRUCTOR " << serial << std::endl;  
  vecData = new int[2];
  if(lst.size() == 2) {
    vecData[0] = *lst.begin();
    vecData[1] = *(1+lst.begin());
  } else {
    // This is an error!!
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
IVec2::IVec2() {
  serial = ++constructSerial;
  std::cout << "NOARG CONSTRUCTOR " << serial << std::endl;
  vecData = new int[2];
  vecData[0] = 0;
  vecData[1] = 0;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
IVec2::IVec2(IVec2 const& iv) {
  serial = ++constructSerial;
  std::cout << "COPY CONSTRUCTOR " << serial << " <- " << iv.serial << std::endl;
  vecData = new int[2];
  vecData[0] = iv.getX();
  vecData[1] = iv.getY();
}  

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef DOMOVEC
IVec2::IVec2(IVec2&& iv) {
  serial = ++constructSerial;
  std::cout << "MOVE CONSTRUCTOR " << serial << " <- " << iv.serial << std::endl;
  vecData = new int[2];
  vecData = iv.vecData;
  iv.vecData = new int[2];   // We do this to preserve the invariant that no IVec2 object has a nullptr for vecData!!
}  
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
IVec2::IVec2(int x, int y) {
  serial = ++constructSerial;
  std::cout << "TWO ARG CONSTRUCTOR " << serial << std::endl;
  vecData = new int[2];
  vecData[0] = x;
  vecData[1] = y;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
IVec2::~IVec2() {
  std::cout << "DESTRUCTOR " << serial << std::endl;
  // NOTE: The only way vecData== nullptr, is if we have a move constructor.
  if(vecData != nullptr)
    delete[] vecData;
}  

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
IVec2& IVec2::operator= (IVec2 const& iv) {
  std::cout << "ASSIGNMENT (COPY) OPERATOR " << serial << " <- " << iv.serial << std::endl;
  if(this != &iv) {
    vecData[0] = iv.getX();
    vecData[1] = iv.getY();
  }
  return *this;  
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef DOMOVEO
IVec2& IVec2::operator=(IVec2&& iv) {
  std::cout << "ASSIGNMENT (MOVE) OPERATOR " << serial << " <- " << iv.serial << std::endl;
  if(this != &iv) {
    int* tmp = vecData;
    vecData = iv.vecData;
    iv.vecData = tmp;     // We do this to preserve the invariant that no IVec2 object has a nullptr for vecData!!
  }
  return *this;  
}
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool IVec2::operator==(IVec2 const& iv) const {
  std::cout << "EQUALITY OPERATOR " << serial << " == " << iv.serial << std::endl;
  return ((getX()==iv.getX()) &&
          (getY()==iv.getY()));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool IVec2::operator<(IVec2 const& iv) const {
  std::cout << "LESS OPERATOR " << serial << " < " << iv.serial << std::endl;
#ifdef DOBAKLESS
  return ((getX()>iv.getX()) ||
          ((getX()==iv.getX()) &&
           (getY()>iv.getY())));
#else
  return ((getX()<iv.getX()) ||
          ((getX()==iv.getX()) &&
           (getY()<iv.getY())));
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int IVec2::getX() const {
  return(vecData[0]);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int IVec2::getY() const {
  return(vecData[1]);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int& IVec2::first() {
  return(vecData[0]);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int& IVec2::second() {
  return(vecData[1]);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int& IVec2::setX(int newX) {
  vecData[0]=newX;
  return vecData[0];
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int& IVec2::setY(int newY) {
  vecData[1]=newY;
  return vecData[1];
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void IVec2::set(int xi, int yi) {
  vecData[0] = xi;
  vecData[1] = yi;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
std::string IVec2::to_s() {
  std::ostringstream convert;
  convert << "(" << vecData[0] << "," << vecData[1] << ")";
  return(convert.str());                                            
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Add ostream support for our object. (12)
std::ostream& operator<< (std::ostream &out, IVec2 &iv) {   
  out << iv.to_s();
  return out;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main () {
  IVec2 aVecA[3];                                       // no-arg constructor x3!
  //std::vector<IVec2> aVecA(3);
  IVec2 a(1,2);                                         // 2 arg constructor
  IVec2 b = IVec2(3,4);                                 // 2 arg constructor & assignment constructor (no zero-arg)
  IVec2 c;                                              // no-arg constructor
  IVec2 d(b);                                           // Copy constructor
  IVec2 aVecB[3] = {{2,1}, {1,2}, {1,3}};               // Construct via an init list (uses 2 arg constructor if list one is missing)

  std::cout << "a: " << a << std::endl;
  std::cout << "b: " << b << std::endl;
  std::cout << "c: " << c << std::endl;
  a = b;                                                // assignment operator
  c.set(5,6);                                           // 2 arg set
  std::cout << "a: " << a << std::endl;
  std::cout << "c: " << c << std::endl;
  std::cout << "d: " << d << std::endl;

  std::cout << "aVecA: " << aVecA[0].to_s() << " " << aVecA[1].to_s() << " " << aVecA[2].to_s() << std::endl;
  aVecA[0]=b;                                                       // assignment operator
  aVecA[1]=IVec2(5,6);                                              // 2 arg constructor & assignment constructor
  aVecA[2].set(7,8);                                                // 2 arg set
  std::cout << "aVecA: " << aVecA[0].to_s() << " " << aVecA[1].to_s() << " " << aVecA[2].to_s() << std::endl;

  std::cout << "aVecB: " << aVecB[0]        << " " << aVecB[1]        << " " << aVecB[2]        << std::endl;  // This time we use the << operator
  std::sort(std::begin(aVecB), std::end(aVecB));
  std::cout << "aVecB: " << aVecB[0] << " " << aVecB[1] << " " << aVecB[2] << std::endl;

  return 0;
 } // end func main
                                                                              
