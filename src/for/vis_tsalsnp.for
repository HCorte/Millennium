C TSALSNP.FOR
C
C V16 09-JUN-2010 RXK Returns displayed
C V15 02-DEC-2000 UXN RELEASE FOR PORTUGAL
C V14 08-JUN-2000 UXN FTNAMES.DEF added.
C V13 14-FEB-2000 OXK Total shown for Sport indexes (Vakio changes )
C V12 13-OCT-1999 RXK Empty line from the middle of display removed.
C V11 17-MAY-1999 UXN Super Triple added.
C V10 05-FEB-1999 UXN Fix for big sales.
C V09 12-DEC-1995 PXB Added new games Doubl and couple
C V08 27-AUG-1995 PXB Reprocessing bug
C V07 26-JUN-1995 PXB V5 changes
C V06 05-MAY-1995 HXK V5 entered into database again!!!!
C V05 21-FEB-1995 PXB Added V5 game.
C V04 05-DEC-1994 HXK Merging from 25-Nov -> 5 Dec
C V03 30-NOV-1994 HXK Fixed accumulating values
C V02 03-JAN-1994 HXK REARRANGED TOTALS.
C V01 24-MAR-1993 SMH Initial Release for Nederland
C
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE TSALSNP(DAT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:FTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

        ! Argument
        INTEGER*4  DAT                              ! CDC date    

	INTEGER*4  SALEN                            !
        PARAMETER (SALEN = NUMTOT * (NUMFIN+1) * MAXGAM)
        INTEGER*4  TLEN                             !
        PARAMETER (TLEN = NUMTOT * (NUMFIN+1) )

        INTEGER*4  LIN                              !
        INTEGER*4  I                                !
        INTEGER*4  GNUM                             ! game number
        INTEGER*4  GIND                             ! game index
        INTEGER*4  GTYP                             ! game type
        INTEGER*4  REC                              ! DAF record to read
        INTEGER*4  K                                ! counter
        INTEGER*4  ST                               ! status
        INTEGER*4  OFF                              ! file descriptor block
        INTEGER*4  FDB(7)                           !
	INTEGER*4  SALES(NUMTOT, NUMFIN+1, MAXGAM)  !
	INTEGER*4  TOTALS(NUMTOT,NUMFIN+1)

	INTEGER*2 D(LDATE_LEN)

        ! start of code

	IF (DAT.LE.0) THEN
            DAT=DAYCDC
        END IF

	D(VCDC)=DAT
	CALL LCDATE(D)
	IF(DAT .EQ. DAYCDC) THEN
	  SMODE=.FALSE.
	  CALL FASTMOV(DAYSTS,DAFSTS,DAFLEN)
	  GOTO 10
	ENDIF

        ! open DAF file
	SMODE=.TRUE.
	CALL OPENW(1,SFNAMES(1,DAF),0,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	  CALL USRCLOS1(1)
	  WRITE(CLIN23,8000) (SFNAMES(K,DAF),K=1,5),ST
	  RETURN
	ENDIF

        ! read DAF record
	REC=DAT
	CALL READW(FDB,REC,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL USRCLOS1(1)
	  WRITE(CLIN23,8001) (SFNAMES(K,DAF),K=1,5),ST,REC
	  RETURN
	ENDIF
	CALL USRCLOS1(1)

        ! check DAF status
	IF(DAFSTS.EQ.DUNUSD) THEN
	  WRITE(CLIN23,8002) (D(K),K=7,13)
	  RETURN
	ENDIF

	IF(DAFSTS.EQ.DNOSAL) THEN
	  WRITE(CLIN23,8003) (D(K),K=7,13)
	  RETURN
	ENDIF

        ! FORMAT SALES SNAPSHOT

10	CONTINUE
	CALL FASTSET(0, SALES, SALEN)
	CALL FASTSET(0, TOTALS, TLEN)
C
C GET TOTAL SALES BY GAME TYPE
C
        DO 30 GNUM = 1, MAXGAM
            IF (GNTTAB(GAMIDX,GNUM) .EQ. 0) GOTO 30
	    GIND = GNTTAB(GAMIDX,GNUM)
	    GTYP = GNTTAB(GAMTYP,GNUM)
	    DO 20 I = 1, NUMFIN+1
	        IF(I.LE.NUMFIN) THEN
	            OFF=I
	            IF(OFF.EQ.TREF) OFF=TVAL
	            SALES(TRACNT,OFF,GNUM) = SALES(TRACNT,OFF,GNUM)+
     *	                                     DAFTYP(TRACNT,I,GNUM)
	            SALES(DOLAMT,OFF,GNUM) = SALES(DOLAMT,OFF,GNUM)+
     *	                                     DAFTYP(DOLAMT,I,GNUM)
	            TOTALS(TRACNT,I)       = TOTALS(TRACNT,I)+
     *	                                     DAFTYP(TRACNT,I,GNUM)
	            TOTALS(DOLAMT,I)       = TOTALS(DOLAMT,I)+
     *	                                     DAFTYP(DOLAMT,I,GNUM)
	        ELSE
	            SALES(TRACNT,I,GNUM) = SALES(TRACNT,I,GNUM)+
     *                                     DAFDIS(TRACNT,GNUM)
	            SALES(DOLAMT,I,GNUM) = SALES(DOLAMT,I,GNUM)+
     *                                     DAFDIS(DOLAMT,GNUM)
	            TOTALS(TRACNT,I)     = TOTALS(TRACNT,I)+
     *                                     DAFDIS(TRACNT,GNUM)
	            TOTALS(DOLAMT,I)     = TOTALS(DOLAMT,I)+
     *                                     DAFDIS(DOLAMT,GNUM)
	        ENDIF
20	    CONTINUE
30	CONTINUE
C
	WRITE(CLIN1,9001) (D(K),K=7,13)
	WRITE(CLIN2,902)

        ! display game information
	LIN = 3
        DO 100 GNUM = 1, MAXGAM
	    IF(GNTTAB(GAMIDX,GNUM) .EQ. 0) GOTO 100
	    GIND = GNTTAB(GAMIDX,GNUM)
	    GTYP = GNTTAB(GAMTYP,GNUM)
            WRITE(XNEW(LIN),903)  GTNAMES(GTYP),GIND,
     *	                          SALES(TRACNT,TWAG,GNUM),	      
     *                            CMONY(SALES(DOLAMT,TWAG,GNUM),12,BETUNIT),
     *                            SALES(TRACNT,TCAN,GNUM),	      
     *                            CMONY(SALES(DOLAMT,TCAN,GNUM),12,BETUNIT),
     *                            SALES(TRACNT,TVAL,GNUM),	      
     *                            CMONY(SALES(DOLAMT,TVAL,GNUM),12,VALUNIT)
	    LIN=LIN+1
                                  
100	CONTINUE

        ! display returns information
        LIN=LIN+1
	WRITE(XNEW(LIN),905)  
        LIN=LIN+1     
        DO GIND = 1,NUMPAS
           GNUM = GTNTAB(TPAS,GIND)    
           WRITE(XNEW(LIN),903)  GTNAMES(TPAS),GIND,
     *	                         SALES(TRACNT,TRET,GNUM),	      
     *                           CMONY(SALES(DOLAMT,TRET,GNUM),12,BETUNIT)
           LIN=LIN+1
        ENDDO
   
        ! display totals information
        LIN=LIN+1
        WRITE(XNEW(LIN),904)    'Total   ',FTNAMES(TWAG),
     *                          TOTALS(TRACNT,TWAG),
     *                          CMONY(TOTALS(DOLAMT,TWAG),12,BETUNIT),
     *                          'Total   ',FTNAMES(TCAN),
     *                          TOTALS(TRACNT,TCAN),	      
     *                          CMONY(TOTALS(DOLAMT,TCAN),12,BETUNIT)

        WRITE(XNEW(LIN+1),904)  'Total   ',FTNAMES(TVAL),
     *                          TOTALS(TRACNT,TVAL),	      
     *                          CMONY(TOTALS(DOLAMT,TVAL),12,BETUNIT),
     *                          'Total   ',FTNAMES(TREF),
     *                          TOTALS(TRACNT,TREF),	      
     *                          CMONY(TOTALS(DOLAMT,TREF),12,VALUNIT)

        WRITE(XNEW(LIN+2),904)  'Total   ',FTNAMES(TRET),
     *                          TOTALS(TRACNT,TRET),	      
     *                          CMONY(TOTALS(DOLAMT,TRET),12,BETUNIT)

        RETURN
C
C
C
C     ================== FORMAT STATEMENTS ================
C

8000	FORMAT(5A4,' file open error ',I4)

8001	FORMAT(5A4,' file read error ',I4,' record - ',I4)

8002	FORMAT('Record not initialized for ',7A2)

8003	FORMAT(7A2,' is not a sales date')
C

9001	FORMAT('Game Sales for ',7A2)

902	FORMAT('TYPE / IND',11X,'SALES',17X,'CANCELS',10X,'CASHS/REFUNDS')

903	FORMAT(A8,I2,2X,I8,1X,A12,I8,1X,A12,I8,1X,A12)

9031	FORMAT(A8,4X,I8,1X,A12,I8,1X,A12,I8,1X,A12)

904	FORMAT(2(2A8,1X,I8,1X,A12,1X))

905	FORMAT(19X,'RETURNS')

	END
