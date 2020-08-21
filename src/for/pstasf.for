C
C SUBROUTINE PSTASF
C $Log:   GXAFIP:[GOLS]PSTASF.FOV  $
C  
C     Rev 1.3   03 Feb 1997 21:05:08   WPW
C  Changes for downloading GVTs.
C  
C     Rev 1.2   13 Jan 1997 16:46:04   RXK
C  GVT id online change added
C  
C     Rev 1.1   28 Nov 1996 23:44:52   WXW
C  Telebetting startup, changes MP/PXN/WXW.
C  Fixes for non commission agents.
C  
C     Rev 1.0   17 Apr 1996 14:33:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.6   12 Feb 1996 12:47:20   RXK
C  Automatic invoice flag now dependent on weekly update count ASFWCT 
C  
C     Rev 1.5   05 Oct 1993 19:15:18   GXA
C  Post Language code as I4 not an I2.
C  
C     Rev 1.4   26 Sep 1993  0:53:34   GXA
C  Posted Agent Language code.
C  
C     Rev 1.3   24 Aug 1993 14:00:04   GXA
C  Added updating of Special and Miscelanious sales.
C  
C     Rev 1.2   13 Jul 1993 14:15:58   SXH
C  Released for Finland
C  
C     Rev 1.1   13 Jun 1993 13:56:04   HXK
C   added AGTINF.DEF, PRMAGT.DEF
C  
C     Rev 1.0   21 Jan 1993 17:23:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pstasf.for **
C
C PSTASF.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE PSTASF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:CLERK.DEF'
C
	INTEGER*4  SUM                      !
	INTEGER*4  J                        !
	INTEGER*4  I                        !
	INTEGER*4  RET                      !
	INTEGER*4  AGT                      !
	INTEGER*4  LCLERK                   !
	INTEGER*4  RECS                     !
	INTEGER*4  K                        !
	INTEGER*4  ST                       !
	INTEGER*4  DOW                      !
	INTEGER*4  WARNING                  !
	INTEGER*4  CLRKFDB(7)               !
	INTEGER*4  OUTLEN                   !
	INTEGER*4  LANG			    !Language Code to post.

	INTEGER*2 DATE(12)

	CHARACTER*132	OUTLINE
C
        ! functions
	INTEGER*4	INANYR
	EXTERNAL	INANYR
C
C
	WARNING = 0
	DATE(VCDC)=DAYCDC
	CALL CDATE(DATE)
	DOW = DATE(VDOW)
	CALL OPENASF(1)
	IF(P(CLRKACT).EQ.0) THEN
	    CALL OPENW(2,SFNAMES(1,CLK),4,0,0,ST)
	    CALL IOINIT(CLRKFDB,2,CLRKSEC*256)
	    IF(ST.NE.0) THEN
	        WRITE(5,903) IAM(),(SFNAMES(K,CLK),K=1,5),ST
	        CALL GPAUSE
	        RETURN
	    ENDIF
C
C POST FINAL CLERK TO CLERK FILE
C
	    WRITE(5,904) IAM(),(SFNAMES(K,CLK),K=1,5)
	    DO 10 RECS=1,NUMAGT

                LCLERK=AGTTAB(AGTNCL,RECS)
                IF(TSBIT(AGTTAB(AGTTYP,RECS),AGTTBA)) LCLERK=1

                IF(LCLERK.EQ.0) GOTO 10    !NO CLERK

	        CALL READW(CLRKFDB,RECS,CLRKREC,ST)
	        IF(ST.NE.0) THEN
	            WRITE(5,905) IAM(),(SFNAMES(K,CLK),K=1,5),ST
	            CALL GPAUSE
	            RETURN
	        ENDIF

	        IF(MOD(RECS,500).EQ.0) TYPE*,IAM(),RECS,' clerks processed'
	        CALL FASTMOV(AGTGAM(1,1,RECS),CLRKDAY(1,1,LCLERK),
     *	                     AGAMLEN*MAXGAM)
		CALL FASTMOV(AGTSPE(1,1,RECS),CLRKSPE(1,1,LCLERK),
     *                       ASPELEN*MAXGAM)
		CALL FASTMOV(AGTMIS(1,1,RECS),CLRKMIS(1,1,LCLERK),
     *                       AMISLEN*NUMTOT)
	        CALL WRITEW(CLRKFDB,RECS,CLRKREC,ST)
	        IF(ST.NE.0) THEN
	            WRITE(5,902) IAM(),(SFNAMES(K,CLK),K=1,5)
	            CALL USRCLOS1(     2)
	            CALL GPAUSE
	            RETURN
	        ENDIF
10	    CONTINUE
	ENDIF
C
C
	WRITE(5,900) IAM(),(SFNAMES(K,ASF),K=1,5)
	DO 1000 AGT=1,NUMAGT
	    CALL READASF(AGT,ASFREC,ST)
	    IF(ST.NE.0) THEN
	        WRITE(5,902) IAM(),(SFNAMES(K,ASF),K=1,5)
	        CALL CLOSASF
	        RETURN
	    ENDIF
C
C
	    IF(P(CLRKACT).EQ.0) THEN
	        CALL READW(CLRKFDB,AGT,CLRKREC,ST)
	        IF(ST.NE.0) THEN
	            WRITE(5,905) IAM(),(SFNAMES(K,CLK),K=1,5),ST
	            CALL GPAUSE
	            RETURN
	        ENDIF
	        IF(MOD(AGT,500).EQ.0) TYPE*,IAM(),AGT,' agents processed'
	    ENDIF
C
C CHECK LAST ASF UPDATE
C
	    IF(ASFDAT(ASFCDC,1).EQ.DAYCDC) GOTO 30
	    IF(ASFDAT(ASFCDC,1).NE.DAYCDC-1.AND.WARNING.NE.1) THEN
	        WARNING=1
	        WRITE(OUTLINE,901) AGT,ASFDAT(ASFCDC,1),DAYCDC
	        OUTLEN=INANYR(OUTLINE,' ')
	        CALL PRMYESNO(OUTLINE(1:OUTLEN),RET)
	        IF(RET.NE.1) THEN
	            WRITE(5,902) IAM(),(SFNAMES(K,ASF),K=1,5)
	            CALL CLOSASF
	            RETURN
	        ELSE
	            CALL PRMYESNO('Continue reporting CDC discrepencies [Y/N] ',
     *                             RET)
	            IF(RET.EQ.1) WARNING=0
	        ENDIF
	    ENDIF
C
C
	    DO I = ANUMDAY,2,-1
	        CALL FASTMOV(ASFDAY(1,1,I-1),ASFDAY(1,1,I),AGAMLEN*MAXGAM)
		CALL FASTMOV(ASFSPE(1,1,I-1),ASFSPE(1,1,I),ASPELEN*MAXGAM)
		CALL FASTMOV(ASFMIS(1,1,I-1),ASFMIS(1,1,I),AMISLEN*NUMTOT)
	        CALL FASTMOV(ASFDAT(1,I-1),ASFDAT(1,I),2)
            END DO
C
C UPDATE WITH TODAYS DATA
C
30	    CONTINUE

	    DO J=1,MAXGAM
	        IF(P(CLRKACT).EQ.0) THEN
	            DO I=1,AGAMLEN
	                SUM=0
	                DO K=1,NUMCLERK
	                    SUM=SUM+CLRKDAY(I,J,K)
                        END DO
	                ASFDAY(I,J,1)=SUM
                    END DO
	        ELSE
	            CALL FASTMOV(AGTGAM(1,J,AGT),ASFDAY(1,J,1),AGAMLEN)
	        ENDIF
	        ASFGFL(J)=AGTGAM(GFLAGS,J,AGT)
            END DO
C
C ACCUMULATE SPECIAL SALES
C
	    DO J = 1,MAXGAM
	       DO I = 1,ASPELEN
	          SUM = 0
		  DO K = 1,NUMCLERK
		     SUM = SUM + CLRKSPE(I,J,K)
		  END DO
		  ASFSPE(I,J,1) = SUM
	       END DO
	    END DO
C
C ACCUMULATE MISCELLANEOUS SALES
C
	    DO J = 1,NUMTOT
	       DO I = 1,AMISLEN
	          SUM = 0
		  DO K = 1,NUMCLERK
		     SUM = SUM + CLRKMIS(I,J,K)
		  END DO
		  ASFMIS(I,J,1) = SUM
	       END DO
	    END DO
C
	    ASFDAT(ASFCDC,1) = DAYCDC
	    ASFDAT(ASFDOW,1) = DOW
	    CALL BINASC(ASFINF,STTYP,LTTYP,AGTTAB(AGTTYP,AGT))
	    LANG = AGTHTB(AGTLANG,AGT)
	    CALL BINASC(ASFINF,SLANG,LLANG,LANG)
            IF(AGTHTB(ASONCT,AGT).GT.0) ASFWCT = ASFWCT + 1
            CALL HTOA(ASFBYT(SGSER),1,LGSER,AGTTAB(AGTGVT1,AGT),ST)
C
C OFFLINE VALIDATIONS
C
C	    IF (AGTHTB(AGTOFFPAY,AGT).GT.0) ASFINV(ASFOFFPAY,1) = AGTHTB(AGTOFFPAY,AGT)
c
C IPS CHANGES
C
	    IF(AGTHTB(AGTCDC,AGT).NE.0) ASFGVT=AGTHTB(AGTCDC,AGT)
	    IF(AGTHTB(AGTCBT,AGT).NE.0) ASFGVTIM=ZEXT(AGTHTB(AGTCBT,AGT))
	    ASFNCDC=ZEXT(AGTHTB(AGTNCDC,AGT))
	    ASFLCDC=ZEXT(AGTHTB(AGTLCDC,AGT))
	    CALL BCLR(AGTTAB(AGTTYP,AGT),AGTCBF)
	    CALL BINASC(ASFINF,STTYP,LTTYP,AGTTAB(AGTTYP,AGT))
C
	    CALL WRITASF(AGT,ASFREC,ST)
	    IF(ST.NE.0) THEN
	        WRITE(5,902) IAM(),(SFNAMES(K,ASF),K=1,5)
	        CALL CLOSASF
	        RETURN
	    ENDIF

1000	CONTINUE

	CALL CLOSASF

	CALL USRCLOS1(     2)

	RETURN
C
C
C
900	FORMAT(1X,A,' Posting sales data to ',5A4)
901	FORMAT(1X,'  Last update for terminal > ',I5,' on ',I4,
     *	 ' Today > ',I4,' Continue update [Y/N]: ')
902	FORMAT(1X,A,1X,5A4,' not updated')
903	FORMAT(1X,A,1X,5A4,' open error ',I4)
904	FORMAT(1X,A,1X,'Posting clerk accounts to ',5A4)
905	FORMAT(1X,A,1X,'Error reading ',5A4,' > ',I4)

	END
