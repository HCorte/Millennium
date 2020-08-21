C  GXSRC:DCORD.FOR
C  
C  $Log:   GXAFIP:[GOLS]DCORD.FOV  $
C  
C 10-JUN-2005 FRP Modify for IPS Distribution.
C
C     Rev 1.3   14 Feb 1997 19:46:32   WPW
C  Changed the game name size from 10 to 6 bytes.
C  
C     Rev 1.2   17 Dec 1996 21:27:36   HXK
C  Reorganised to handle  i) games available request
C                        ii) games defined request (at signon)
C  
C     Rev 1.1   05 Dec 1996 20:32:20   HXK
C  Updated for Finland IPS pre-release
C  
C     Rev 1.0   17 Apr 1996 12:49:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   22 Sep 1994  7:56:52   MCM
C  START INDEX AT 2 (BYTE 1 IS RESERVED)
C  
C     Rev 1.2   08 Sep 1994 12:17:46   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 20:09:44   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:36:12   SYSTEM
C  Initial revision.
C
C V02 11-FEB-92 JPJ ADDED (GVT)
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE CROSS SYSTEM GAME PARAMETERS MESSAGE.
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DCORD(TRABUF,OUTTAB,CRSBUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CRSBUF.DEF'
C
        INTEGER*4 IND, LENGTH, I
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C GET LENGTH
C
	IND=2
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	LENGTH=I4TEMP
C
C CHECK ERROR CODE
C
        IND=20
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	TRABUF(TIERR)=I4TEMP
	IF(TRABUF(TIERR).GT.INOER) GOTO 8000
C
C GET CHECKSUM
C
	IND=12
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        CRSBUF(GTBCHK)=I4TEMP
C
C GET RETAILER CREDIT LIMIT
C
	IND=14
        I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	I1TEMP(3)=OUTTAB(IND+2)
	I1TEMP(4)=OUTTAB(IND+3)
	CRSBUF(GTBRCL)=I4TEMP
	TRABUF(TGPRCL)=I4TEMP
C
C GET NUMBER OF GAMES
C
	IND=18
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        CRSBUF(NBRENT)=I4TEMP
        IF(CRSBUF(NBRENT).GT.MAXGTB) CRSBUF(NBRENT)=1
C
        IF(LENGTH.NE.22+CRSBUF(NBRENT)*36) THEN
           TRABUF(TIERR)=INLTH
           GOTO 8000
        ENDIF
C
C GET NEXT GAME NUMBER
C
	IND=22
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        CRSBUF(NXTGAM)=I4TEMP
        IND=IND+2
C
C GET GAME NUMBER
C
	DO 100 I=0,CRSBUF(NBRENT)-1
C
	   I4TEMP=0
	   I1TEMP(1)=OUTTAB(IND+0)
	   I1TEMP(2)=OUTTAB(IND+1)
	   CRSBUF(GTBGAM+I)=I4TEMP
	   IND=IND+2
C
C GET SHORT GAME NAME
C
	   CRSBUF(GTBNM1+I)=0
	   CALL MOVBYT(OUTTAB(IND+0),1,CRSBUF(GTBNM1+I),1,4)
	   IND=IND+4

	   CRSBUF(GTBNM2+I)=0
	   CALL MOVBYT(OUTTAB(IND+0),1,CRSBUF(GTBNM2+I),1,2)
	   IND=IND+2
C
C GET LONG GAME NAME
C
	   CRSBUF(GTBNM3+I)=0
	   CALL MOVBYT(OUTTAB(IND+0),1,CRSBUF(GTBNM3+I),1,4)
	   IND=IND+4

	   CRSBUF(GTBNM4+I)=0
	   CALL MOVBYT(OUTTAB(IND+0),1,CRSBUF(GTBNM4+I),1,4)
	   IND=IND+4

	   CRSBUF(GTBNM5+I)=0
	   CALL MOVBYT(OUTTAB(IND+0),1,CRSBUF(GTBNM5+I),1,4)
	   IND=IND+4

	   CRSBUF(GTBNM6+I)=0
	   CALL MOVBYT(OUTTAB(IND+0),1,CRSBUF(GTBNM6+I),1,4)
	   IND=IND+4

	   CRSBUF(GTBNM7+I)=0
	   CALL MOVBYT(OUTTAB(IND+0),1,CRSBUF(GTBNM7+I),1,4)
	   IND=IND+4
C
C GET FLAGS
C
	   I4TEMP=0
	   I1TEMP(1)=OUTTAB(IND+0)
	   I1TEMP(2)=OUTTAB(IND+1)
	   CRSBUF(GTBFLG+I)=I4TEMP
	   IND=IND+2
C
C GET TICKET COST
C
	   I4TEMP=0
	   I1TEMP(1)=OUTTAB(IND+0)
	   I1TEMP(2)=OUTTAB(IND+1)
	   I1TEMP(3)=OUTTAB(IND+2)
	   I1TEMP(4)=OUTTAB(IND+3)
	   CRSBUF(TKTPRC+I)=I4TEMP
	   IND=IND+4
C
C GET TICKETS PER PACK
C
	   I4TEMP=0
	   I1TEMP(1)=OUTTAB(IND+0)
	   I1TEMP(2)=OUTTAB(IND+1)
	   CRSBUF(TKTPCK+I)=I4TEMP
	   IND=IND+2
C
100	CONTINUE
C
8000	CONTINUE
	RETURN

	END
