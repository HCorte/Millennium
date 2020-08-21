C
C SUBROUTINE X2CHK_DIST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHK_DIST.FOV                               $
C  $Date::   17 Apr 1996 16:13:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V1.0	  24 OCT 1994 DXG
C
C This program verifies that the distribution of network ports among
C the station is correct by calculating the number of times the port
C is assigned to a specific station. This should (approximately) the
C same for each port.
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
	PROGRAM X2CHK_DIST
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETCOM.DEF'
C
	INTEGER*4   STN                             !Station number
	INTEGER*4   I,J,BASE,TEMP,K
	INTEGER*4   PORTCNT(1:X2X_MAXPRT_ASSIGN,0:X2X_NETWORK_PORTS)
	INTEGER*4   SITECNT(X2X_MAX_SITES,1:X2X_MAXPRT_ASSIGN)
	INTEGER*4   SITE,CLASS,TOTAL
	INTEGER*4   NUM_STNS(2)
C
C
C
	DO J=1,X2X_NETWORK_PORTS
	  DO I=1,X2X_MAXPRT_ASSIGN
	       PORTCNT(I,J) = 0
	  END DO
	END DO
C
	DO J=1,X2X_MAXPRT_ASSIGN
	  DO I=1,X2X_MAX_SITES
	       SITECNT(I,J) = 0
	  END DO
	END DO
C
C
	
	BASE = 259
	DO 2000,STN=1,X2X_STATIONS
	  CLASS = X2XS_STNCLS(STN)
	  IF (X2XS_STNCLS(STN).NE.2) GOTO 2000
	   NUM_STNS(MOD(STN,2)+1) = NUM_STNS(MOD(STN,2)+1)+1
	  DO I=1,2     ! X2X_MAXPRT_ASSIGN
             SITE=X2XC_SITE(I,X2XS_STNCLS(STN))
C
C         Set the sit to the modulus of max sites if
C         the optimize flag is set
C
             IF((SITE.NE.0).AND.(X2XC_OPTIMIZE_SITES(CLASS).EQ.
     *         X2XC_ENABLE_OPTIMIZE_SITES)) THEN
C               SITE=MOD(STN+SITE,X2X_MAX_SITES)+1
               SITE=MOD(SITE,X2X_MAX_SITES)+1
C
	     TEMP = MAX0(ALL_SETS_OF_PORTS(SITE,STN,I) - BASE,0)
	     TEMP = MIN0(TEMP,X2X_NETWORK_PORTS)
	       PORTCNT(I,TEMP) = PORTCNT(I,TEMP)+1
	       SITECNT(SITE,I) = SITECNT(SITE,I)+1
             ENDIF
C
C	TYPE *,SITE,STN,I
	  END DO
2000	CONTINUE
C
C
	DO I=1,X2X_MAXPRT_ASSIGN
	  TOTAL = 0
	  DO J=1,X2X_NETWORK_PORTS-BASE
	       WRITE (6,9000) J+BASE, PORTCNT(I,J),I
	       TOTAL = TOTAL + PORTCNT(I,J)
	  END DO
	  WRITE (6,9300) TOTAL,TOTAL/(X2X_NETWORK_PORTS-BASE)
	END DO
C
	DO I=1,X2X_MAXPRT_ASSIGN
	       WRITE (6,9100)( K,SITECNT(K,I),K=1,X2X_MAX_SITES)
	END DO
C
	WRITE (6,9200) NUM_STNS(1),NUM_STNS(2)

C
C
9000	FORMAT (' NET PRT ',I4,' ASSIGNED ',I8,
     *	      ' TIMES FOR ENTRY ',I4)
9100	FORMAT (2(' SITE ',I4,' COUNT: ',I8))
9200	FORMAT ('NUM OF ODD STNS : ',I6,' NUM OF EVEN STNS: ',I6)
9300	FORMAT ('TOTAL: ',I8,' AVERAGE: ',I8)
	END
