/**
   @file      groupsC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Typical use of communicators and groups in MPI.@EOL
   @Keywords  MPI groups communicators
   @Std       C99

              This program illistrates a typical use of MPI
              communicators and MPI groups.

              Not all platforms allow console I/O on all MPI
              processes.  On such platforms, this program can fail.
*/

#include "mpi.h"
#include <stdio.h>              /* I/O lib         ISOC  */

int main(int argc, char *argv[]) {
  int        oldRank, newRank, oldSize, newSize;
  int        grpMembers1[1024], grpMembers2[1024]; // Assume <1024 tasks
  int        grpSize1, grpSize2;
  int        i, n;
  MPI_Group  oldGroup, newGroup;
  MPI_Comm   newComm;

  /* initialize MPI.. */
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &oldRank);
  MPI_Comm_size(MPI_COMM_WORLD, &oldSize);

  /* Build our group membership lists. */
  for(i=grpSize1=grpSize2=0;i<oldSize;i++)
    if((i%2)==0)
      grpMembers1[grpSize1++] = i;
    else
      grpMembers2[grpSize2++] = i;

  /* Display the group memberships. */
  if(oldRank == 0) {
    printf("Group 1 (%d members): ", grpSize1);
    for(i=0;i<grpSize1;i++)
      printf("%d ", grpMembers1[i]);
    printf("\n");

    printf("Group 2 (%d members): ", grpSize2);
    for(i=0;i<grpSize2;i++)
      printf("%d ", grpMembers2[i]);
    printf("\n");
  } /* end if */

  /* Figure out our current group. */
  MPI_Comm_group(MPI_COMM_WORLD, &oldGroup);

  /* Divide tasks into two groups based upon even/odd ranks. */
  if ((oldRank%2)==0)
    MPI_Group_incl(oldGroup, grpSize1, grpMembers1, &newGroup);
  else
    MPI_Group_incl(oldGroup, grpSize2, grpMembers2, &newGroup);

  /* Create new communicators */
  MPI_Comm_create(MPI_COMM_WORLD, newGroup, &newComm);

  /* Get our rank and the size of the new communicator groups. */
  MPI_Comm_rank(newComm, &newRank);
  MPI_Comm_size(newComm, &newSize);

  /* Bcast our original rank from the new rank zero(s) */
  n=oldRank;
  MPI_Bcast(&n, 1, MPI_INT, // ptr to data, num elements, type
            0,              // process with master copy
            newComm);       // group to send data too  

  /* Print out some stuff to help us understand what is going on. */
  printf("oldRank=%03d/%03d newRank=%03d/%03d myRankZero=%03d\n",oldRank, oldSize, newRank, newSize, n);

  /* Free up our group and communicator.  It is common for programs to
     not do this becasue the resources for groups and communciators
     will be relesed upon program exit. */
  MPI_Group_free(&newGroup);
  MPI_Comm_free(&newComm);

  /* Finish up. */
  MPI_Finalize();
} /* end func main */
