C  GXSRC:DCMNU.FOR
C  
C V03 14-JUN-2005 FRP Modify for IPS Distribution.
C
C  $Log:   GXAFXT:[GOLS]DCMNU.FOV  $
C  
C     Rev 1.1   17 Dec 1996 21:22:50   HXK
C  Changed to handle actual ticket orders from terminals
C  
C     Rev 1.0   17 Apr 1996 12:49:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   22 Sep 1994  7:56:12   MCM
C  START INDEX AT 2 (BYTE 1 IS RESERVED)
C  
C     Rev 1.2   08 Sep 1994 12:16:14   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 20:09:28   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:36:04   SYSTEM
C  Initial revision.
C
C V02 11-FEB-92 JPJ ADDED (GVT)
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE CROSS SYSTEM MENU MESSAGE.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DCMNU(TRABUF,OUTTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, LENGTH, I
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C
C GET LENGTH
C
        IND=2
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        LENGTH=I4TEMP
C
C GET ORDER NUMBER
C
        IND=12
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        I1TEMP(3)=OUTTAB(IND+2)
        I1TEMP(4)=OUTTAB(IND+3)
        TRABUF(TSORD)=I4TEMP
        IND=IND+4
C
C
C CHECK ERROR CODE
C
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        TRABUF(TIERR)=I4TEMP
        IF(TRABUF(TIERR).GT.INOER) GOTO 8000
        IF(LENGTH.NE.(22+TRABUF(TIBCH))) THEN
           TRABUF(TIERR)=INLTH
           GOTO 8000
        ENDIF
        IND=IND+2
C
C GET ERROR INFO
C
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        I1TEMP(3)=OUTTAB(IND+2)
        I1TEMP(4)=OUTTAB(IND+3)
        TRABUF(TSINF)=I4TEMP
        IND=IND+4
C
C NUMBER OF ENTRIES (TIBCH)
C
        IND=IND+2
C
C GET OUT OF STOCK FLAGS
C
        DO 100 I=0,TRABUF(TIBCH)-1
           I4TEMP=0
           I1TEMP(1)=OUTTAB(IND+0)
           IF(I4TEMP.EQ.49) THEN  !49 DEC = 31 HEX = 1 ASCII
             CALL BSET(TRABUF(TSSTK1),I)
           ELSE
             CALL BCLR(TRABUF(TSSTK1),I)
           ENDIF
           IND=IND+1
100     CONTINUE
C
C
C
8000	CONTINUE
	RETURN
	END
