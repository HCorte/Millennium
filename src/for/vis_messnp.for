C MESSNP.FOR
C                                                                               
C V03 31-MAR-2000 OXK Increased # of pages to 7 (Vakio changes)
C V02 01 Feb 1997 RXK Changes for CDU.
C V01 16-JUL-1991 MTK INITITAL RELEASE FOR FINLAND
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
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
        SUBROUTINE MESSNP(PAGE)                                        
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'                                    
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'           
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'

        ! argument
        INTEGER*4  PAGE        !

	! parameterS
	INTEGER*4  MAXPAGE
	PARAMETER (MAXPAGE = 7)

        ! variables
        INTEGER*4  MESTAB(TICKET_LENGTH, TICKET_ROWS, 
     *                    MAXGAM+PRM_NUMOPN+NUMCDU) 
        INTEGER*4  MESS((MAXGAM+PRM_NUMOPN+NUMCDU)*TICKET_LENGTH*
     *                   TICKET_ROWS)   
        INTEGER*4  ISTAB(10,TICKET_ROWS+1,MAXGAM+PRM_NUMOPN+NUMCDU+1)
        INTEGER*4  I                                           !
        INTEGER*4  K                                           !
        INTEGER*4  J                                           !
        INTEGER*4  GIND                                        !
        INTEGER*4  GTYP                                        !
        INTEGER*4  IND                                         !
        INTEGER*4  BLANK                                       !
        INTEGER*4  BEG_OFFSET
        INTEGER*4  END_OFFSET

        CHARACTER*40  STAB(TICKET_ROWS+1,MAXGAM+PRM_NUMOPN+NUMCDU+1)
C
        DATA BLANK/'    '/                                             
        EQUIVALENCE (MESS,MESTAB) 
        EQUIVALENCE (STAB, ISTAB)                                       
C                                                                               
        INTEGER*4 LASTGAM
        CHARACTER CDUTEXT(1:NUMCDU)*21
        DATA CDUTEXT/'CDU Portuguese text 1','CDU Portuguese text 2',
     *               'CDU Portuguese text 3','CDU Portuguese text 4'/

      INTEGER*4 ITEMPTAB(8)
      CHARACTER CTEMPTAB(32)
      EQUIVALENCE(ITEMPTAB,CTEMPTAB) 
C
      INTEGER*4    AE/Z000000C4/,OE/Z000000D6/,AO/Z000000C5/         ! Ä,Ö,Å
      CHARACTER*1  AE_TERM/'5B'X/,OE_TERM/'5C'X/,AO_TERM/'5D'X/      ! [,|,]
      CHARACTER*1  AE_CHAR,OE_CHAR,AO_CHAR
      EQUIVALENCE  (AE,AE_CHAR),(OE,OE_CHAR),(AO,AO_CHAR)

      INTEGER*4    AE2/Z000000E4/,OE2/Z000000F6/,AO2/Z000000E5/      ! ä,ö,å
      CHARACTER*1  AE2_TERM/'7B'X/,OE2_TERM/'7C'X/,AO2_TERM/'7D'X/   ! {,|,}
      CHARACTER*1  AE2_CHAR,OE2_CHAR,AO2_CHAR
      EQUIVALENCE  (AE2,AE2_CHAR),(OE2,OE2_CHAR),(AO2,AO2_CHAR)

C
C
        SMODE=.TRUE.                                                   
        CALL FASTMOV(TKTMES(1,1,1), MESTAB(1,1,1), 
     *       TICKET_LENGTH*TICKET_ROWS*(MAXGAM+PRM_NUMOPN+NUMCDU))    

        DO I = 1, (MAXGAM+PRM_NUMOPN+NUMCDU)*TICKET_LENGTH*TICKET_ROWS
            IF(MESS(I).EQ.0) MESS(I)=BLANK                             
        END DO
        CALL FASTSET(BLANK,ISTAB,10*(TICKET_ROWS+1)*
     *              (MAXGAM+PRM_NUMOPN+NUMCDU))       
C                                                                               
C                                                                               
        ! CDU texts
        DO I = 1, NUMCDU
            WRITE(STAB(1,I),802) CDUTEXT(I)
            DO J = 1, TKTMLN(MAXGAM+PRM_NUMOPN+1)
                CALL FASTMOV(MESTAB(1,J,MAXGAM+PRM_NUMOPN+I),ITEMPTAB,5)
                DO K=1,20 
                   IF(CTEMPTAB(K).EQ.AE_TERM) CTEMPTAB(K)=AE_CHAR
                   IF(CTEMPTAB(K).EQ.AO_TERM) CTEMPTAB(K)=AO_CHAR
                   IF(CTEMPTAB(K).EQ.OE_TERM) CTEMPTAB(K)=OE_CHAR
                   IF(CTEMPTAB(K).EQ.AE2_TERM) CTEMPTAB(K)=AE2_CHAR
                   IF(CTEMPTAB(K).EQ.AO2_TERM) CTEMPTAB(K)=AO2_CHAR
                   IF(CTEMPTAB(K).EQ.OE2_TERM) CTEMPTAB(K)=OE2_CHAR
                ENDDO
                WRITE(STAB(1+J,I),801) J,(ITEMPTAB(K),K=1,5)
            END DO
        END DO    

        !game ticket texts
        DO 30 I = 1, MAXGAM                                  
            IF(GNTTAB(GAMIDX,I).EQ.0) GOTO 30                 
            GIND = GNTTAB(GAMIDX,I)                           
            GTYP = GNTTAB(GAMTYP,I)                           
            IF(GTYP.NE.0) LASTGAM=I
            WRITE(STAB(1,I+NUMCDU),800) GTNAMES(GTYP),GIND,   
     *                              (GLNAMES(K,I),K=1,4)      
            DO J = 1, TKTMLN(I)
                CALL FASTMOV(MESTAB(1,J,I),ITEMPTAB,8)
                DO K=1,30 
                   IF(CTEMPTAB(K).EQ.AE_TERM) CTEMPTAB(K)=AE_CHAR
                   IF(CTEMPTAB(K).EQ.AO_TERM) CTEMPTAB(K)=AO_CHAR
                   IF(CTEMPTAB(K).EQ.OE_TERM) CTEMPTAB(K)=OE_CHAR
                   IF(CTEMPTAB(K).EQ.AE2_TERM) CTEMPTAB(K)=AE2_CHAR
                   IF(CTEMPTAB(K).EQ.AO2_TERM) CTEMPTAB(K)=AO2_CHAR
                   IF(CTEMPTAB(K).EQ.OE2_TERM) CTEMPTAB(K)=OE2_CHAR
                ENDDO
                WRITE(STAB(1+J,NUMCDU+I),801) J,(ITEMPTAB(K),K=1,8)
            END DO
30      CONTINUE                                              
C                                       
        LASTGAM = LASTGAM + MOD(LASTGAM,2) + NUMCDU    !last used even number
        ! opinion polls
        DO I = 1, PRM_NUMOPN              
            WRITE(STAB(1,I+LASTGAM),8000) I
            DO J = 1, TKTMLN(MAXGAM+I)
                CALL FASTMOV(MESTAB(1,J,MAXGAM+I),ITEMPTAB,8)
                DO K=1,30 
                   IF(CTEMPTAB(K).EQ.AE_TERM) CTEMPTAB(K)=AE_CHAR
                   IF(CTEMPTAB(K).EQ.AO_TERM) CTEMPTAB(K)=AO_CHAR
                   IF(CTEMPTAB(K).EQ.OE_TERM) CTEMPTAB(K)=OE_CHAR
                   IF(CTEMPTAB(K).EQ.AE2_TERM) CTEMPTAB(K)=AE2_CHAR
                   IF(CTEMPTAB(K).EQ.AO2_TERM) CTEMPTAB(K)=AO2_CHAR
                   IF(CTEMPTAB(K).EQ.OE2_TERM) CTEMPTAB(K)=OE2_CHAR
                ENDDO
                WRITE(STAB(1+J,LASTGAM+I),801) J,(ITEMPTAB(K),K=1,8)
            ENDDO
        END DO    

        IF (PAGE .LT. 1 .OR. PAGE .GT. MAXPAGE) PAGE = 1
	BEG_OFFSET = 10*(PAGE-1) +  1
	END_OFFSET = 10*(PAGE-1) + 10

        WRITE(CLIN1,901)                 
        IND=3                            
        DO I = BEG_OFFSET, END_OFFSET, 2                 
            DO K = 1, TICKET_ROWS+1
                IF(I.GT.(MAXGAM+NUMCDU+PRM_NUMOPN)) THEN
                   DO J=IND,22
                      WRITE(XNEW(J),904)
                   ENDDO
                   RETURN
                ELSE
                   WRITE(XNEW(IND),902) STAB(K,I), STAB(K,I+1)
                END IF
                IND = IND + 1
            END DO
            IF(IND.GT.19) RETURN                                               
        END DO

        RETURN                           
C                                                                               
C                                                                               
800     FORMAT(5X,A8,1X,I1,4X,4A4)       
8000    FORMAT(5X,'Opinion Poll ',I2)
801     FORMAT(2X,I1,2X,8A4)             
802     FORMAT(5X,A21)
901     FORMAT('Ticket message snapshot')
902     FORMAT(A40,A40)
903     FORMAT(A40)                                                         
904     FORMAT(80(' '))
C
        END
