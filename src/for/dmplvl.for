C PROGRAM VIEWLVL
C
C V01 16-MAY-2000 OXK  INITIAL RELEASE 
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM DMPLVL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
	INCLUDE 'INCLIB:DESLVL.DEF'
C
	INTEGER*4   LVLIM
	PARAMETER(LVLIM=168)
C
        INTEGER*4   I, J, K, L, DUMMY, ST, LEVEL, CNT
        CHARACTER   FLINE*80
        INTEGER*4   LEV(0:15,5,LVLIM+5)
	INTEGER*4   LVL(5)

        INTEGER*4 INLUN/2/
        INTEGER*4 OUTLUN/3/

	CHARACTER*15 LEVNAM(5,0:SNPCNT)	    ! 1ST IND : SNP,CMD,HASF,UPAS,CHGL
					    ! 2ND IND : # OF THE 'ITEM'

C REFER TO DESLVL.DEF TO CHECK SNPCNT, CMDCNT, HSFCNT, USRCNT & CHGCNT 
C	TO USE THEM CORRECTLY HERE

	DO I=1,5
	    DO J=0,SNPCNT
		LEVNAM(I,J)='               '
	    ENDDO
	ENDDO
	DO I=1,SNPCNT
	    IF (I.LE.SNPCNT) LEVNAM(1,I)=SNPNAM(I)
	    IF (I.LE.CMDCNT) LEVNAM(2,I)=CMDNAM(I)
	    IF (I.LE.HSFCNT) LEVNAM(3,I)=HASFNAM(I)
	    IF (I.LE.USRCNT) LEVNAM(4,I)=USENAM(I)
	    IF (I.LE.CHGCNT) LEVNAM(5,I)=CHGNAM(I)
	ENDDO

	CALL FASTSET(0,LEV,16*5*LVLIM)
C
C Open LEVEL file
C
      OPEN(INLUN,FILE='GXTSK:LEVEL.FIL',IOSTAT=ST,
     *	 STATUS='OLD',RECL=80, BLOCKSIZE=512,READONLY,
     *	 ORGANIZATION='SEQUENTIAL',
     *	 ACCESS='SEQUENTIAL')
      IF(ST.NE.0) THEN
      	 WRITE(6,910)
         CALL USRCLOS1(INLUN)
	 GOTO 200
      ENDIF
C
C Open REPORT file
C
        CALL ROPEN('GXTSK:DMPLVL.REP',OUTLUN,ST)
        IF (ST.NE.0) THEN
            CALL USRCLOS1(OUTLUN)
            GOTO 200
        ENDIF
C
C Read level file to array lev(level,program,1-168)
C
50      CONTINUE
        READ(INLUN,920,END=65) FLINE
        IF(FLINE(1:2).EQ.'/*') GOTO 65
        IF(FLINE(1:4).EQ.'****') THEN
           LEVEL=CTOI(FLINE(5:6),DUMMY)
           CNT = 0
           GOTO 50
        ENDIF
        CALL ENCLVL(LEVEL,FLINE)
        CNT = CNT + 1
	IF (CNT.GT.LVLIM) THEN
	    WRITE(6,900)
	    GOTO 65
	ENDIF
	READ(FLINE,930)LEV(LEVEL,1,CNT),	! vision snap
     *		       LEV(LEVEL,2,CNT),	! vision comm
     *		       LEV(LEVEL,3,CNT),	! hasf
     *		       LEV(LEVEL,4,CNT),	! userpass
     *		       LEV(LEVEL,5,CNT)         ! chglvl
        GOTO 50

65	CONTINUE
        CALL USRCLOS1(INLUN)
C
C Print out the selected level information.
C
	WRITE(OUTLUN,940)
	DO I=1,5
	  DO LEVEL=0,15
      	      DO J=1,CNT,5
		  IF (MAX(LEV(LEVEL,I,J),LEV(LEVEL,I,J+1),
     *                    LEV(LEVEL,I,J+2),LEV(LEVEL,I,J+3),
     *                    LEV(LEVEL,I,J+4)) .GT. 0) THEN
		     DO L=1,5
			LVL(L) = 0
			IF (I.EQ.2) THEN
			    DO K=1,CMDCNT
				IF (CMDNUM(K).EQ.LEV(LEVEL,I,J+L-1)) LVL(L)=K
			    ENDDO
			ELSE
			    LVL(L)=LEV(LEVEL,I,J+L-1)
			ENDIF
		     ENDDO
		     WRITE(OUTLUN,950)I,LEVEL,
     *			  LEV(LEVEL,I,J)  ,LEVNAM(I,LVL(1)),
     *			  LEV(LEVEL,I,J+1),LEVNAM(I,LVL(2)),
     *			  LEV(LEVEL,I,J+2),LEVNAM(I,LVL(3)),
     *			  LEV(LEVEL,I,J+3),LEVNAM(I,LVL(4)),
     *			  LEV(LEVEL,I,J+4),LEVNAM(I,LVL(5))
	          ENDIF
	      ENDDO
	  ENDDO
	ENDDO

C
C
C Exit
C
200     CONTINUE
C
900	FORMAT(10X,'Too many level descriptions found in LEVEL.FIL!',/,
     *	       10X,'Skipping the rest of the file!')
910     FORMAT(10X,'LEVEL.FIL not found!')
920     FORMAT(A80)
930	FORMAT(I3,1X,I5,1X,I2,1X,I2,1X,I2)
940	FORMAT(1X,'Prog Lvl',5(4x,'Id:Name',11X))
950     FORMAT(3X,I1,2X,I2,1X,5(1X,I5.0,':',A15))
        END
