C
C V02 15-JUN-2000 UXN Cleaned up.
C V01 xx-xxx-xxxx RXK Initial release.
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
	SUBROUTINE TRPTOP(TRABUF)  
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'

        INTEGER*4 UCID		      !unic combination identifier
        INTEGER*4 MONY		      !bet amount per 1 cmb
        INTEGER*4 SIGN		      !+1 if wager, -1 if cancel
        INTEGER*4 GIND		      !game index

	INTEGER*4 PUCID,NUCID	      !previous and next links for ucid,here
				      !previous means "previous bigger amount" 			
				      !and next means "next smaller amount" 			
	INTEGER*4 LAST		      !the closest entry number in top table
				      !with bigger amount than UCID
	INTEGER*4 PREV,NEXT	      !previous and next links for LAST

        INTEGER*4 I1,I2,I3
        INTEGER*4 OFF1,OFF2,OFF3
        INTEGER*4 EVE(3)              !simple bet for events 

        LOGICAL TABTOP		      !entry is on the top of the link table
        LOGICAL BOTTOM		      !entry is on the bottom of the link table


        GIND=TRABUF(TGAMIND)
        MONY=TRABUF(TWAMT)
        IF(TRABUF(TWSYST).EQ.FULSYS) MONY=MONY/TRABUF(TWSYSN)
        SIGN=1
        IF(TRABUF(TTYP).EQ.TCAN.OR.TRABUF(TTYP).EQ.TINC) SIGN=-1

        OFF1=-1
  	OFF2=OFF1+TRABUF(TWTTMA)
  	OFF3=OFF2+TRABUF(TWTTMB)

        DO 1030 I1=1,TRABUF(TWTTMA)
           DO 1020 I2=1,MAX(1,TRABUF(TWTTMB))
	      DO 1010 I3=1,MAX(1,TRABUF(TWTTMC))
                 EVE(1) = TRABUF(TWTTBET+OFF1+I1)
	         UCID   = EVE(1)

	         IF(TRPEST(2,GIND).EQ.GAMOPN) THEN
                    EVE(2) = TRABUF(TWTTBET+OFF2+I2)
	            UCID   = EVE(1) + (EVE(2)-1)*MAXTRPRW
	            IF(TRPEST(3,GIND).EQ.GAMOPN) THEN
                       EVE(3) = TRABUF(TWTTBET+OFF3+I3)
		       UCID   = EVE(1) + (EVE(2)-1)*MAXTRPRW +
     *                          (EVE(3)-1)*MAXTRPRW*MAXTRPRW
                    ENDIF
                 ENDIF
C
C MAINTENANCE OF TOP TABLE BEGINS
C
      	TRODDS(TRGAMT,UCID,GIND) = TRODDS(TRGAMT,UCID,GIND) + SIGN*MONY

	IF(TRODDS2(TRGPEL,UCID,GIND).NE.0 .OR.
     *	   TRODDS2(TRGNEL,UCID,GIND).NE.0) THEN
C
C UPDATE NECESSARY LINKS
C IF WAGER THEN SEARCH FOR THE CLOSEST BIGGER AMOUNT IN BIGGER-DIRECTION
C
           IF(SIGN*MONY.GT.0) THEN  
	      IF(UCID.EQ.TROFEL(GIND)) THEN
		 GOTO 1000
	      ELSEIF(TRODDS(TRGAMT,UCID,GIND).LE.
     *		 TRODDS(TRGAMT,TRODDS2(TRGPEL,UCID,GIND),GIND)) THEN
                 IF(UCID.EQ.TROLEL(GIND))
     *		    TROLAMT(GIND) = TROLAMT(GIND) + MONY
		 GOTO 1000 	
	      ENDIF	
              TABTOP=.FALSE.
              BOTTOM=.FALSE. 
              IF(UCID.EQ.TROLEL(GIND)) THEN
     	         BOTTOM=.TRUE.
 	      ENDIF	 
              LAST=TRODDS2(TRGPEL,UCID,GIND) 
C
C FIND THE CLOSEST BIGGER AMOUNT
C
130           CONTINUE
	      IF(TRODDS(TRGAMT,UCID,GIND).GE.TRODDS(TRGAMT,LAST,GIND)) THEN
                 IF(LAST.EQ.TROFEL(GIND)) THEN 
                    TABTOP=.TRUE.
                    GOTO 140
                 ENDIF    
                 LAST=TRODDS2(TRGPEL,LAST,GIND)
                 GOTO 130
              ENDIF
C
C UPDATE LINKS, IF NECESSARY UPDATE LINKS TO FIRST AND LAST ENTRY AND
C LOWEST TOP COMBINATION AMOUNT ALSO
C
140           CONTINUE
	      LAST=TRODDS2(TRGNEL,LAST,GIND)  
              NEXT=TRODDS2(TRGNEL,LAST,GIND)
              PREV=TRODDS2(TRGPEL,LAST,GIND)
              PUCID=TRODDS2(TRGPEL,UCID,GIND)
              NUCID=TRODDS2(TRGNEL,UCID,GIND)

              IF(TABTOP.AND.BOTTOM)THEN
                 TRODDS2(TRGNEL,PUCID,GIND)=NUCID
                 TRODDS2(TRGPEL,UCID,GIND)=  0
                 TRODDS2(TRGNEL,UCID,GIND)=PREV
                 TRODDS2(TRGPEL,PREV,GIND)=UCID
                 TROFEL(GIND)=UCID
                 TROLEL(GIND)=PUCID
                 TROLAMT(GIND)=TRODDS(TRGAMT,PUCID,GIND)

              ELSEIF(TABTOP)THEN
                 TRODDS2(TRGPEL,NUCID,GIND)=PUCID
                 TRODDS2(TRGNEL,PUCID,GIND)=NUCID
                 TRODDS2(TRGPEL,UCID,GIND)=  0
                 TRODDS2(TRGNEL,UCID,GIND)=PREV
                 TRODDS2(TRGPEL,PREV,GIND)=UCID
                 TROFEL(GIND)=UCID

              ELSEIF(BOTTOM)THEN
                 TRODDS2(TRGNEL,PUCID,GIND)=NUCID
                 TRODDS2(TRGNEL,UCID,GIND)=LAST
                 TRODDS2(TRGPEL,UCID,GIND)=PREV
                 TRODDS2(TRGPEL,LAST,GIND)=UCID
                 TRODDS2(TRGNEL,PREV,GIND)=UCID
                 TROLEL(GIND)=PUCID
                 TROLAMT(GIND)=TRODDS(TRGAMT,PUCID,GIND)

	      ELSE	
                 TRODDS2(TRGNEL,PUCID,GIND)=NUCID               
                 TRODDS2(TRGPEL,NUCID,GIND)=PUCID               
                 TRODDS2(TRGPEL,UCID,GIND)=PREV
	         TRODDS2(TRGNEL,UCID,GIND)=LAST
	         TRODDS2(TRGPEL,LAST,GIND)=UCID
	         TRODDS2(TRGNEL,PREV,GIND)=UCID  
              ENDIF
              GOTO 1000    !end for wager
C
C IF CANCELLATION THEN SEARCH FOR THE CLOSEST BIGGER AMOUNT IN SMALLER-DIRECTION
C
           ELSE
              IF(TRODDS(TRGAMT,UCID,GIND).EQ.0) THEN
		 IF(UCID.EQ.TROLEL(GIND)) THEN
		    TROLEL(GIND)=TRODDS2(TRGPEL,UCID,GIND) 		    
		    TROLAMT(GIND)=TRODDS(TRGAMT,TROLEL(GIND),GIND)
		    TRODDS2(TRGNEL,TROLEL(GIND),GIND)=0
     		    TRODDS2(TRGPEL,UCID,GIND)=0
     		    TRODDS2(TRGNEL,UCID,GIND)=0
		 ELSEIF(UCID.EQ.TROFEL(GIND)) THEN
		    TROFEL(GIND)=TRODDS2(TRGNEL,UCID,GIND)
		    TRODDS2(TRGPEL,TROFEL(GIND),GIND)=0
		    TRODDS2(TRGPEL,UCID,GIND)=0
                    TRODDS2(TRGNEL,UCID,GIND)=0
		 ELSE
		    TRODDS2(TRGPEL,TRODDS2(TRGNEL,UCID,GIND),GIND)=
     *			   TRODDS2(TRGPEL,UCID,GIND)
		    TRODDS2(TRGNEL,TRODDS2(TRGPEL,UCID,GIND),GIND)=
     *			   TRODDS2(TRGNEL,UCID,GIND)
     		    TRODDS2(TRGPEL,UCID,GIND)=0
     		    TRODDS2(TRGNEL,UCID,GIND)=0
		 ENDIF     
                 TROTNUM(GIND) = TROTNUM(GIND) -1
                 GOTO 1000
              ENDIF
              IF(UCID.EQ.TROLEL(GIND)) THEN
		 TROLAMT(GIND)=TROLAMT(GIND)-MONY
		 GOTO 1000
	      ENDIF	 
              IF(TRODDS(TRGAMT,UCID,GIND).GE.
     *           TRODDS(TRGAMT,TRODDS2(TRGNEL,UCID,GIND),GIND)) THEN
                 GOTO 1000
              ENDIF
	    
              TABTOP=.FALSE.
              BOTTOM=.FALSE. 
              IF(UCID.EQ.TROFEL(GIND)) THEN
                 TABTOP=.TRUE.
              ENDIF
              LAST=TRODDS2(TRGNEL,UCID,GIND) 
C
C FIND THE CLOSEST BIGGER AMOUNT
C
150           CONTINUE

              IF(TRODDS(TRGAMT,UCID,GIND).GT.TRODDS(TRGAMT,LAST,GIND))THEN     
                 LAST=TRODDS2(TRGPEL,LAST,GIND)
                 GOTO 160
              ELSE
                 IF(LAST.EQ.TROLEL(GIND)) THEN
                    BOTTOM=.TRUE.
                    GOTO 160
                 ENDIF
                 LAST=TRODDS2(TRGNEL,LAST,GIND)
                 GOTO 150
              ENDIF
CC
C UPDATE LINKS, IF NECESSARY UPDATE LINKS TO FIRST AND LAST ENTRY AND
C LOWEST TOP COMBINATION AMOUNT ALSO
C
160           CONTINUE

              NEXT=TRODDS2(TRGNEL,LAST,GIND)
              PREV=TRODDS2(TRGPEL,LAST,GIND)
              PUCID=TRODDS2(TRGPEL,UCID,GIND)
              NUCID=TRODDS2(TRGNEL,UCID,GIND)

              IF(TABTOP.AND.BOTTOM)THEN
                 TRODDS2(TRGPEL,NUCID,GIND)= 0
	         TRODDS2(TRGNEL,LAST,GIND)=UCID
                 TRODDS2(TRGNEL,UCID,GIND)=NEXT
	         TRODDS2(TRGPEL,UCID,GIND)=LAST
		 TROLEL(GIND)=UCID
		 TROFEL(GIND)=NUCID
      	         TROLAMT(GIND)=TRODDS(TRGAMT,UCID,GIND)   
              ELSEIF(TABTOP)THEN
                 TRODDS2(TRGPEL,NUCID,GIND)= 0
                 TRODDS2(TRGPEL,UCID,GIND)=LAST
	         TRODDS2(TRGNEL,UCID,GIND)=NEXT
	         TRODDS2(TRGNEL,LAST,GIND)=UCID  
	         TRODDS2(TRGPEL,NEXT,GIND)=UCID  
		 TROFEL(GIND)=NUCID
              ELSEIF(BOTTOM)THEN
                 TRODDS2(TRGNEL,PUCID,GIND)=NUCID               
                 TRODDS2(TRGPEL,NUCID,GIND)=PUCID               
                 TRODDS2(TRGNEL,UCID,GIND)=NEXT
	         TRODDS2(TRGPEL,UCID,GIND)=LAST
	         TRODDS2(TRGNEL,LAST,GIND)=UCID
		 TROLEL(GIND)=UCID
      	         TROLAMT(GIND)=TRODDS(TRGAMT,UCID,GIND)
	      ELSE	
                 TRODDS2(TRGNEL,PUCID,GIND)=NUCID               
                 TRODDS2(TRGPEL,NUCID,GIND)=PUCID               
                 TRODDS2(TRGPEL,UCID,GIND)=LAST
	         TRODDS2(TRGNEL,UCID,GIND)=NEXT
	         TRODDS2(TRGPEL,NEXT,GIND)=UCID
	         TRODDS2(TRGNEL,LAST,GIND)=UCID  
              ENDIF
              GOTO 1000    !end for cancellation
           ENDIF          
C
C VERY FIRST ENTRY
C 
        ELSEIF(TROTNUM(GIND).EQ.0) THEN
           TROTNUM(GIND)=1
           TROFEL(GIND)=UCID
           TROLEL(GIND)=UCID
           TRODDS2(TRGPEL,UCID,GIND)=0
           TRODDS2(TRGNEL,UCID,GIND)=0
	   TROLAMT(GIND)=TRODDS(TRGAMT,UCID,GIND)
C
C NEXT NEW ENTRIES
C
        ELSEIF(TROTNUM(GIND).EQ.1.AND.UCID.EQ.TROLEL(GIND)) THEN
	   TROLAMT(GIND)=TROLAMT(GIND)+SIGN*MONY
           IF(TROLAMT(GIND).EQ.0) THEN
     	      TRODDS2(TRGPEL,UCID,GIND)=0
              TRODDS2(TRGNEL,UCID,GIND)=0
              TROTNUM(GIND)=0
           ENDIF
	   GOTO 1000
 	ELSE
           TROTNUM(GIND)=TROTNUM(GIND)+1
           IF(TROLAMT(GIND).GE.TRODDS(TRGAMT,UCID,GIND)) THEN !new bottom value
              TRODDS2(TRGNEL,UCID,GIND)=0
              TRODDS2(TRGPEL,UCID,GIND)=TROLEL(GIND)
	      TRODDS2(TRGNEL,TROLEL(GIND),GIND)=UCID	
              TROLEL(GIND)=UCID
	      TROLAMT(GIND)=TRODDS(TRGAMT,UCID,GIND)
           ELSEIF(TRODDS(TRGAMT,UCID,GIND).GT.		      !new top value
     *            TRODDS(TRGAMT,TROFEL(GIND),GIND)) THEN
	      TRODDS2(TRGPEL,TROFEL(GIND),GIND)=UCID	
              TRODDS2(TRGPEL,UCID,GIND)=0
              TRODDS2(TRGNEL,UCID,GIND)=TROFEL(GIND)
              TROFEL(GIND)=UCID
           ELSE		                                      !middle value
	      LAST=TROLEL(GIND)
230	      CONTINUE
              IF(TRODDS(TRGAMT,UCID,GIND).GT.TRODDS(TRGAMT,LAST,GIND)) THEN
                 LAST=TRODDS2(TRGPEL,LAST,GIND)
                 GOTO 230
              ENDIF
	      TRODDS2(TRGPEL,UCID,GIND)=LAST	
	      TRODDS2(TRGNEL,UCID,GIND)=TRODDS2(TRGNEL,LAST,GIND)
	      TRODDS2(TRGPEL,TRODDS2(TRGNEL,LAST,GIND),GIND)=UCID	      
	      TRODDS2(TRGNEL,LAST,GIND)=UCID
           ENDIF 
        ENDIF
C
1000    CONTINUE

1010    CONTINUE
1020	CONTINUE
1030	CONTINUE

        RETURN

        END

