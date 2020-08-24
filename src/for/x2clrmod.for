C
C SUBROUTINE X2CLRMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CLRMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:13:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2clrmod.for;1 **
C
C X2CLRMOD.FTN
C
C V02 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V01 18-SEP-91 RRB INITIAL RELEASE.
C
C This program will clear the field modification indication bitmap
C for the specified file.
C
C Input parameters:
C
C     FILE   -    File offset as defined in X2XFILES.DEF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE X2CLRMOD(FILE)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:X2XFIL.DEF'
C
      INTEGER*4   ALLREC(128)                 !Record buffer
      INTEGER*4   FILE                        !File number
      INTEGER*4   FILIDX                      !File Index
      INTEGER*4   REC                         !Record number
      CHARACTER   X2FILNAM*20                 !File Name Function
      INTEGER*4   X2FILSEC                    !File Sector Function
      INTEGER*4   FDB(7)                      !File Descriptor Block
      LOGICAL     BIGREAD                     !Block read mode
      INTEGER*4   ST
C
      DO 100 FILIDX = 1,X2XFIL_MAX_FILES
        IF(X2XFIL_FILE_LIST(FILIDX).EQ.FILE) GOTO 200
100   CONTINUE
C
      TYPE*,IAM(),'X2CLRMOD: File not found in X2XFIL.DEF.....'
      GOTO 400
C
200   CONTINUE
      REC=0
      BIGREAD=.FALSE.
      WRITE(5,9000) IAM(),X2FILNAM(FILE)
      IF(X2FILSEC(FILE).EQ.2) THEN
        CALL OPENX2X(X2FILNAM(FILE),1)
        BIGREAD=.TRUE.
      ELSE
        CALL OPENX(1,X2FILNAM(FILE),4,0,0,ST)
        IF(ST.NE.0)THEN
          CALL OS32ER(5,X2FILNAM(FILE),'OPENX',ST,0)
          PAUSE
        ENDIF
        CALL IOINIT(FDB,1,X2FILSEC(FILE)*256)
      ENDIF
C
C READ THROUGH ENTIRE FILE SKIPPING EMPTY SLOTS.
C
300   CONTINUE
        REC=REC+1
        IF(BIGREAD) THEN
          CALL READX2X(1,REC,ALLREC,ST)
        ELSE
          CALL READW(FDB,REC,ALLREC,ST)
          IF(ST.NE.0 .AND. ST.NE.144) THEN
            CALL OS32ER(5,X2FILNAM(FILE),'READW',ST,REC)
            PAUSE
          ENDIF
        ENDIF
C
C IF NO MORE RECORDS, CLOSE THE CURRENT FILE.
C
        IF(ST.EQ.144) THEN
          IF(BIGREAD) THEN
            CALL CLOSX2X(1)
          ELSE
            CALL CLOSEFIL(FDB)
          ENDIF
          REC=0
          GOTO 400
        ENDIF
C
C CLEAR BITMAPS (If not already cleared).
C
        IF(ALLREC(X2XFIL_BITMAP(FILIDX)).EQ.0.AND.
     *     ALLREC(X2XFIL_BITMAP(FILIDX)+1).EQ.0 .AND.		!V02
     *     ALLREC(X2XFIL_BITMAP(FILIDX)+2).EQ.0 .AND.		!V02
     *     ALLREC(X2XFIL_BITMAP(FILIDX)+3).EQ.0) GOTO 300	!V02
        ALLREC(X2XFIL_BITMAP(FILIDX))  =0
        ALLREC(X2XFIL_BITMAP(FILIDX)+1)=0
        ALLREC(X2XFIL_BITMAP(FILIDX)+2)=0			!V02
        ALLREC(X2XFIL_BITMAP(FILIDX)+3)=0			!V02
C
C REWRITE RECORD
C
        IF(BIGREAD) THEN
          CALL WRITX2X(1,REC,ALLREC,ST)
        ELSE
          CALL WRITEW(FDB,REC,ALLREC,ST)
          IF(ST.NE.0) THEN
            CALL OS32ER(5,X2FILNAM(FILE),'WRITEW',ST,REC)
            PAUSE
          ENDIF
        ENDIF
C
C READ THE NEXT RECORD
C
      GOTO 300
400   CONTINUE
C
      RETURN
C
C     ================== Format Statements =====================
C
9000  FORMAT(1X,A,'Clearing field update flags for file ',A20)
      END
