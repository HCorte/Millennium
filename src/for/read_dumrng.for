C SUBROUTINE READ_DUMRNG
C 
C V05 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V04 22-FEB-1996 wsm X2X Upgrade: Added AGTINF.DEF for Finland.
C V03 02-MAR-1996 wsm FIX STNDEF.FIL PATH
C V02 09-NOV-1995 WJK USE SYSTEM FLAG TO DETERMINE STNDEF.FIL PATH
C V01 01-AUG-1994 SCD INITIAL RELEASE FOR UK
C
C This program will read the Station definition file and process only the
C dummy station ranges.  Any other records will be skipped.  The routine has
C been patterned after READ_STNDEF.  The record formats that are valid in
C READ_STNDEF are also valid, if ignored, for this subroutine.
C 
C File format for dummy station records:
c
C #  STATION RANGE FILE
C #  FORMAT ('D',1X,I4,2X,I5,2X,I5)  for DUMMY station ranges.
C #
C #       START   END
C #  SCL   STN    STN
C D 0001  00000  00000            # DUMMY STATIONS FOR X.21 (UNUSED)
C D 0002  00000  00000            # DUMMY STATIONS FOR X.25 (UNUSED)
C D 0003  00001  00002            # DUMMY STATIONS FOR ASYNC (UNUSED)
C D 0004  00000  00000            # DUMMY STATIONS FOR ISOCH (DIGITAL) (UNUSED)
C D 0005  00000  00000            # DUMMY STATIONS FOR USAT (UNUSED)
C D 0006  00000  00000            # DUMMY STATIONS FOR X25PVC (UNUSED)
C D 0007  00003  00003            # DUMMY STATIONS FOR GTECH DIAL
C D 0008  00010  00100            # DUMMY STATIONS FOR GTX DIAL (UNUSED)
C D 0009  00005  00005            # DUMMY STATIONS FOR X.28 PAD
C
C
CC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
        SUBROUTINE READ_DUMRNG
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:X2BLDNET.DEF'
C
        CHARACTER*1  CHR80(80)
        CHARACTER*4  CHR_CLASS
        CHARACTER*5  CHR_START
        CHARACTER*5  CHR_LAPB
        CHARACTER*5  CHR_END
        CHARACTER*5  CHR_STN
        CHARACTER*50 CHR_EQUIV
        CHARACTER*16 CHR_X25ADR
C
        CHARACTER   STNDEF_NAME*20      ! V02 Global Setup file name
	INTEGER*4   STNI4_NAME(5)	! V02
        EQUIVALENCE (STNDEF_NAME,STNI4_NAME)
	
        INTEGER*4   I, J, K, ST
        INTEGER*4   START,END,XCLASS
        INTEGER*4   DUMMY_LOCAL_STN_CLASS(X2XC_CLASSES)	!Local vector - only
					!used to determine multiple dummy
					!station records for a given class
C
C
        EQUIVALENCE (CHR80(3),CHR_CLASS)
        EQUIVALENCE (CHR80(3),CHR_LAPB)
        EQUIVALENCE (CHR80(9),CHR_START)
        EQUIVALENCE (CHR80(16),CHR_END)
        EQUIVALENCE (CHR80(3),CHR_STN)
        EQUIVALENCE (CHR80(10),CHR_X25ADR)
        EQUIVALENCE (CHR80(23),CHR_EQUIV)
C
C       Clear variables  
C
        CALL FASTSET(0,DUMMY_LOCAL_STN_CLASS,X2XC_CLASSES)
        CALL FASTSET(0,X2XC_DUMMY_START_STN,X2XC_CLASSES)
        CALL FASTSET(0,X2XC_DUMMY_END_STN,X2XC_CLASSES)
C
C DETERMINE STNDEF.FIL PATH BASED OF SYSTEM FLAG V02
C
        IF (PX2X_TASK) THEN                                     ! V02
          STNDEF_NAME = 'PX2X'//'FILE'//'S:ST'//'NDEF'//'.FIL'  ! VO2
        ELSE                                                    ! VO2
          STNDEF_NAME = 'STND'//'EF.F'//'IL  '//'    '//'    '  ! VO2
        ENDIF                                                   ! V02
C
C       Open the station definition file
C
        CALL OPENW(2,STNI4_NAME,4,0,0,ST)      ! JFH - ALPHA 
        IF(ST.NE.0) THEN
          CALL OS32ER(5,'STNDEF.FIL','OPENW',ST,0)
          CALL GPAUSE
        ENDIF
C
C       Read the station ranges for each class's DUMMY stations
C
10      CONTINUE
        CHR80(1) = ' '
        READ(2,9001,END=50) CHR80
C
C       dummy station addresses start with 'D' or 'd'
C
        IF (CHR80(1).EQ.'D' .OR. CHR80(1).EQ.'d') THEN
          XCLASS = CTOI(CHR_CLASS,K)
          START  = CTOI(CHR_START,K)
          END    = CTOI(CHR_END,K)
C
C         Determine if class is multiply defined
C
          IF(DUMMY_LOCAL_STN_CLASS(XCLASS).NE.0) THEN
            TYPE *,'READ_DUMRNG: Dummy Stations for Class ',XCLASS, 
     *			 ' are multiply defined'
	    GOTO 10
          ELSE
            DUMMY_LOCAL_STN_CLASS(XCLASS) = XCLASS
          ENDIF
C
C         Are classes declared within the valid ranges.
C
          IF(XCLASS.LT.1 .OR. XCLASS.GT.X2XC_CLASSES) THEN
            TYPE *,'READ_DUMRNG: Invalid DUMMY station class: ',XCLASS
	    GOTO 10
C
C         Classes may be declared in file but have no stations
C         associated with them just ignore the record
C
          ELSEIF(START.LE.0 .OR. END.LE.0) THEN
	    TYPE *,'READ_DUMRNG: Invalid DUMMY station range: ',START,' - ',END
            TYPE *,'READ_DUMRNG: for station class: ',XCLASS,
     *                   ' Record ignored ' 
            GOTO 10
C
C         Station number exceeds maximum number configured
C
          ELSEIF(START.GT.X2X_STATIONS .OR. END.GT.X2X_STATIONS) THEN
	    TYPE *,'READ_DUMRNG: DUMMY Station range assigned exceeds ',
     *		    X2X_STATIONS
	    GOTO 10
C
C         Station ranges have been declared for a class
C
          ELSEIF(START.GT.0 .AND. END.LE.X2X_STATIONS .AND.
     *           END .GE. START) THEN
            X2XC_DUMMY_START_STN(XCLASS) = START
            X2XC_DUMMY_END_STN(XCLASS)   = END
	    X2XC_DUMMY_STN_COUNT(XCLASS) = X2XC_DUMMY_END_STN(XCLASS) -
     *					   X2XC_DUMMY_START_STN(XCLASS) + 1
C
          ENDIF
C
        ENDIF

        GOTO 10

50      CONTINUE
C
C
C       Check for any overlapping DUMMY station ranges
C
        DO 65 I = 1,X2XC_PERM_CLASSES
	   IF(X2XC_DUMMY_START_STN(I).EQ.0 .OR. X2XC_DUMMY_END_STN(I).EQ.0) 
     *	      GOTO 65 
           DO 60 J = I+1,X2XC_PERM_CLASSES
              IF(J.EQ.I) GOTO 60
              IF((X2XC_DUMMY_START_STN(J).GE.X2XC_DUMMY_START_STN(I) 
     *		 .AND. X2XC_DUMMY_END_STN(J) .LE.X2XC_DUMMY_END_STN(I)) .OR.
     *		 (X2XC_DUMMY_START_STN(J).LE.X2XC_DUMMY_START_STN(I) 
     *		 .AND. X2XC_DUMMY_END_STN(J) .GE.X2XC_DUMMY_END_STN(I))) THEN
		TYPE *,'READ_DUMRNG: Range for DUMMY stations for class ',J,
     *			     ' invalid.'
		TYPE *,'READ_DUMRNG: DUMMY Stations Already defined for class ',I
              ENDIF
60         CONTINUE
65      CONTINUE 
C
C
        CLOSE(2)
C
C       Format statements
C
9001    FORMAT(80A1)
        RETURN
        END
