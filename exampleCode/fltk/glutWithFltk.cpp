// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      glutWithFltk.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1993,1997,2003 by Mitch Richling.  All rights reserved.
   @brief     Simple example of FLTK's GLUT compatibility features@EOL
   @Keywords  fltk glut
   @Std        FLTKv1.3

   FLTK has a nice GLUT compatibility layer that makes creating sophisticated OpenGL programs a snap.  Many GLUT programs will
   compile out of the box with nothing more than a change in include path!  This tiny program is such an example -- it is a GLUT
   program with the include path changed.
***********************************************************************************************************************************/

/**********************************************************************************************************************************/
#include <FL/glut.H>            /* FLTK GLUT       FLTK  */

/**********************************************************************************************************************************/
static float r1 = 1, r2 = 0, r3 = 0;

/**********************************************************************************************************************************/
void displayCall() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glRotatef(0.5, r1, r2, r3);
  glCallList(1);
  glutSwapBuffers();
}

/**********************************************************************************************************************************/
void idleCall() {
  static int c = 0;
  c++;
  if(c > 100) {
    c = 0;
    if(r1 > 0) {
      r1 = 0; r2 = 1; r3 = 0;
    } else if(r2 > 0) {
      r1 = 0; r2 = 0; r3 = 1;
    } else if(r3 > 0) {
      r1 = 1; r2 = 0; r3 = 0;
    }
  }
  glutPostRedisplay();
}

/**********************************************************************************************************************************/
void computeCurve() {
  int maxBalls    = 5000;
  double tDelta   = 0.01;
  double x = 0.11;
  double y = 0.0;
  double z = 0;
  double a = 10;
  double b = 28;
  double c = 8.0 / 3.0;
  int numBalls;
  double xNew, yNew, zNew;
  glNewList(1, GL_COMPILE);
  glColor3f(1,1,1);
  glPointSize(2.0);
  glBegin(GL_LINE_STRIP);
  for(numBalls=0;numBalls<maxBalls;numBalls++) {
    xNew = x + a*(y-x)*tDelta;
    yNew = y + (x*(b-z)-y)*tDelta;
    zNew = z + (x*y-c*z)*tDelta;
    glColor3f(1.0*numBalls/maxBalls, 0.2, 1.0-numBalls/maxBalls);
    glVertex3f(xNew, yNew, zNew);    
    x=xNew;
    y=yNew;
    z=zNew;
  }
  glEnd();
  glEndList();
}

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
  glutInitWindowSize(500, 500);
  glutInitWindowPosition(10, 10);
  glutCreateWindow("GLUT Compatibility Mode Demo Program");
  glutDisplayFunc(displayCall);
  glutIdleFunc(idleCall);
  glEnable(GL_DEPTH_TEST);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-2.0, 2.0, -2.0, 2.0, -2.0, 500.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glScalef(.03,.03,.03);
  computeCurve();
  glutMainLoop();
  return 0;
}
