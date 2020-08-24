C  GXSRC:INILVL.FOR
C  
C  $Log:   GXAFXT:[GOLS]INILVL.FOV  $
C  
C     Rev 1.1   19 May 1996 17:53:38   HXK
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    19 May 1996 17:49:06   HXK
C  Initial revision.
C  
C
C SUBROUTINE INILVL
C
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_altrlvl.for **
C
C INILVL.FOR
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INILVL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESLVL.DEF'
C
	INTEGER*4   LVLIM
	PARAMETER   (LVLIM=168)
C
	INTEGER*4   I, J, K, ST, ANS
	CHARACTER   FLINE*80, SPACES*80
	CHARACTER   CNUM*5, COMMA
	CHARACTER*5 LEV(0:15,5,LVLIM)
	DATA COMMA/','/
C
C Initializing
C
	I=0
	DO 5 I=1,80
	  SPACES(I:I)=' '
	  FLINE(I:I)=' '
5	CONTINUE
	J=0
	K=0
	DO 30 J=0,15
	  DO 20 I=1,5
	    DO 10 K=1,LVLIM
	      LEV(J,I,K)='00000'
10	    CONTINUE
20	  CONTINUE
30	CONTINUE
	CNUM='     '
C
C Make sure he/she wants it
C
        CALL WIMG(5,'Do you really want to do this ?  [Y/N] ')
        CALL YESNO(ANS)
        IF(ANS.EQ.1) THEN
	  GOTO 40
        ELSE
	  GOTO 200
        ENDIF
C
C Open LEVEL file
C
40	CONTINUE
	OPEN(9,FILE='GXPROJ:[TSK]LEVEL.FIL',IOSTAT=ST,
     *       STATUS='NEW',RECL=80, BLOCKSIZE=512,
     *       ORGANIZATION='SEQUENTIAL',
     *       ACCESS='SEQUENTIAL')
C
        REWIND(9)
	DO 50 I=15,14,-1
	  FLINE(1:80)=SPACES
	  FLINE(1:4)='****'
	  CALL PAD0(CNUM,I,2,' ')
	  FLINE(5:6)=CNUM(1:2)
	  FLINE(7:7)=COMMA
          WRITE(9,910)FLINE
	  DO 60 J=1,LVLIM
	    FLINE(1:80)=SPACES
	    FLINE(1:3)=LEV(I,1,J)	    !write vision snap
	    FLINE(4:4)=COMMA
	    FLINE(5:9)=LEV(I,2,J)	    !write vision commands
	    FLINE(10:10)=COMMA	
	    FLINE(11:12)=LEV(I,3,J)	    !write hasf
	    FLINE(13:13)=COMMA
	    FLINE(14:15)=LEV(I,4,J)	    !write userpass
	    FLINE(16:16)=COMMA
	    FLINE(17:18)=LEV(I,5,J)	    !write chglvl  
    	    CALL ENCLVL(I,FLINE)
	    WRITE(9,910)FLINE
60	  CONTINUE
50	CONTINUE
        FLINE(1:80)=SPACES
	FLINE(1:2)='/*'	  
        WRITE(9,910)FLINE
C
C Exit
C
200     CONTINUE
        CALL XWAIT(1,2,ST)
        CALL CLRSCR(5)
        CALL USRCLOS1(9)
        RETURN
C
910	FORMAT(A80)
	END
