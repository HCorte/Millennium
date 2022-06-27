      PROGRAM GMGN
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'  
        INCLUDE 'INCLIB:QUECOM.DEF'  
        INCLUDE 'INCLIB:CONCOM.DEF'    
C        INCLUDE 'OLMCON.DEF'

        CHARACTER YN, ALLONE, BLOCKBUF
        LOGICAL RUNPROG /.TRUE./ 
        INTEGER*4 DISLST(1024), OLMLST(1024)
        INTEGER*4 I, LIMIT, USED
        INTEGER*4	RESULT
        INTEGER*4 OPT,ST
        CHARACTER OPTION
        INTEGER*4 BUFNUM, STAT        
      
11      CONTINUE
        IF(RUNPROG) THEN
          PRINT *, 'To Exit GMGN (General Management) press e'
          PRINT *, 'To remove/free all buffers in use in MILL press (1) A'
          PRINT *, 'To remove/free a block of buffer in use in MILL press (2) O'
          PRINT *, 'See All the Buffers in the App Queue press (3) W'
          PRINT *, 'See the List of Active Tasks press (4) T'
          PRINT *, 'Reset the Active Task List for all Tasks press (5) R'
          PRINT *, '(TEST) Incremente Active Task for COMOLM (6) I'
          PRINT *, '(TEST) Decrement Active Task for COMOLM (7) D'
          PRINT *, 'List all the buffers in the OLM queue (8) Q'
          PRINT *, 'Remove Buff from OLM queue (9) ...'
C          PRINT *, 'THIS PROGRAM IS TO REMOVE A BUFFER FROM APPLICATION QUEUE, CONTINUE THE PROGRAMA (Y/N) or W for watch?'
          READ *, OPTION

          IF(OPTION .EQ. 'E' .OR. OPTION .EQ. 'e') THEN
C            RETURN
          ENDIF          

          OPT = ICHAR(OPTION) - ICHAR('0')
C          CALL INPNUM('Enter option [C - bring up the system]',OPT
C          *              , 1
C          *              , 9
C          *              , ST)          

          GOTO(100,200,300,400,500,600,700,800,900) OPT

100       CONTINUE
            CALL RALLBUFFS_FROMAPPQUEUE(QUETAB(1,DIS),.FALSE.)
          GOTO 11  
          
200       CONTINUE
            CALL RALLBUFFS_FROMAPPQUEUE(QUETAB(1,DIS),.TRUE.)
          GOTO 11  

300       CONTINUE
            CALL LISTSIZE(QUETAB(1,DIS),USED)       
            CALL QIMAGE(QUETAB(1,DIS),DISLST,USED)
            PRINT *, 'DISPATCHER'
            PRINT 102,DISLST(1)
            DO I=2, USED+1
              PRINT 103,DISLST(I) 
            ENDDO
          GOTO 11  

400       CONTINUE
            DO I=1, NUMAPPQUE
              PRINT *,'I:',I
              PRINT *,'ACTTSK(I):',ACTTSK(I)
            ENDDO
          GOTO 11  

500       CONTINUE
            DO I=1, NUMAPPQUE
              ACTTSK(I)=0
            ENDDO
          GOTO 11  

600       CONTINUE
            CALL LIB$ADAWI(1, ACTTSK(29), RESULT)
            PRINT *,'RESULT:',RESULT
          GOTO 11  

700       CONTINUE
            CALL LIB$ADAWI(-1, ACTTSK(29), RESULT)
            PRINT *,'RESULT:',RESULT
          GOTO 11 
          
800       CONTINUE
           CALL LISTSIZE(COMOLMQUE(1),USED)       
           CALL QIMAGE(COMOLMQUE(1),OLMLST,USED)
           PRINT *, 'OLIMPO Queue'
           PRINT *, 'USED BUFFERS:',USED
           PRINT 104,OLMLST(1)
           DO I=2, 10
C           DO I=2, USED+1
             PRINT 105,OLMLST(I) 
           ENDDO
          GOTO 11 
          
900       CONTINUE
            CALL QIMAGE(COMOLMQUE(1),OLMLST,USED)
            DO I=1,OLMLST(1)
              CALL RTL (BUFNUM, COMOLMQUE(1), STAT)
              IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
                PRINT *, 'LIST iS EMPTY'
                BUFNUM = 0
              ENDIF 
              CALL RELBUF(BUFNUM)
            ENDDO
          GOTO 11 
          
C          IF(YN .EQ. 'Y' .OR. YN .EQ. 'y') THEN
C            PRINT *, 'REMOVE ALL BUFFER (A or a) OR ONE BUFFER (O or o)'
C            READ *, ALLONE
C            IF(ALLONE .EQ. 'A' .OR. ALLONE .EQ. 'a') THEN   
C                PRINT *, 'REMOVE ALL (A) or BLOCK OF BUFFS (B)'  
C                READ *, BLOCKBUF
C                IF(BLOCKBUF .EQ. 'A' .OR. BLOCKBUF .EQ. 'a') THEN           
C                  CALL RALLBUFFS_FROMAPPQUEUE(QUETAB(1,DIS),.FALSE.)
C                ELSE 
C                  CALL RALLBUFFS_FROMAPPQUEUE(QUETAB(1,DIS),.TRUE.)       
C                ENDIF
C            ELSEIF (ALLONE .EQ. 'O' .OR. ALLONE .EQ. 'o') THEN
C                CALL RBUFF_FROMAPPQUEUE(QUETAB(1,DIS))
C            ELSE 
C                CALL RBUFF_FROMAPPQUEUE(QUETAB(1,DIS))
C            ENDIF
C            GOTO 11
C          ELSEIF(YN .EQ. 'W' .OR. YN .EQ. 'w') THEN     
C            CALL LISTSIZE(QUETAB(1,DIS),USED)       
C            CALL QIMAGE(QUETAB(1,DIS),DISLST,USED)
C            PRINT *, 'DISPATCHER'
C            PRINT 100,DISLST(1)
C            DO I=2, USED+1
C              PRINT 101,DISLST(I) 
C            ENDDO   
C            GOTO 11   
C          ELSEIF(YN .EQ. 'T' .OR. YN .EQ. 't') THEN
C            DO I=1, NUMAPPQUE
C              PRINT *,'I:',I
C              PRINT *,'ACTTSK(I):',ACTTSK(I)
C            ENDDO
C            GOTO 11
C          ELSEIF(YN .EQ. 'R' .OR. YN .EQ. 'r') THEN
C            DO I=1, NUMAPPQUE
C              ACTTSK(I)=0
C            ENDDO
C            GOTO 11   
C          ELSEIF(YN .EQ. 'A' .OR. YN .EQ. 'a') THEN
C            CALL LIB$ADAWI(1, ACTTSK(29), RESULT)
C            PRINT *,'RESULT:',RESULT
C            GOTO 11  
C          ELSEIF(YN .EQ. 'D' .OR. YN .EQ. 'd') THEN
C            CALL LIB$ADAWI(-1, ACTTSK(29), RESULT)
C            PRINT *,'RESULT:',RESULT
C            GOTO 11                        
C          ENDIF
C          IF(YN .EQ. 'N' .OR. YN .EQ. 'n') THEN
C            RUNPROG = .FALSE.
C          ENDIF
        ENDIF
C        PRINT *,'PROGRAM AS ENDED...'

102	FORMAT(1X,'<',I4.0,'>')
103	FORMAT(2X,I4.0) 
104	FORMAT(1X,'<',I4.0,'>')
105	FORMAT(2X,I4.0)        

      END

      SUBROUTINE RBUFF_FROMAPPQUEUE(LIST)
        IMPLICIT NONE  
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'   
        INCLUDE 'INCLIB:TASKID.DEF'  
        INCLUDE 'INCLIB:QUECOM.DEF'
	      INCLUDE 'INCLIB:GLIST.DEF'                     
C        INCLUDE 'OLMCON.DEF'

        INTEGER*4 OPT,ST, BUFNUM, STAT
	      INTEGER*4 LIST(*)        
C        CHARACTER QUEUE
        INTEGER*4 QUEUE, APPQUEUESIZE

10	CONTINUE    


        CALL LISTSIZE(QUETAB(1,DIS),APPQUEUESIZE)
        PRINT *, 'APP QUEUE SIZE:', APPQUEUESIZE

        PRINT *, 'ENTER BUFFER NUM TO REMOVE AND FREE:'
        READ *, BUFNUM
        PRINT *, BUFNUM
C        PRINT *, 'ENTER QUEUE NAME FROM WHICH THE BUFFER IS AT THE MOMENT:'
C        READ *, QUEUE
C        PRINT *, QUEUE

C        QUEUE = QUETAB(1,DIS)


        CALL RTL (BUFNUM, QUETAB(1,DIS), STAT)
        IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
          BUFNUM = 0
        ENDIF 
        CALL RELBUF(BUFNUM)
        
        PRINT *, 'REMOVEL STATUS:', STAT
        IF(STAT .EQ. GLIST_STAT_EMPTY) PRINT *, 'APPLICATION QUEUE LIST EMPTY...'
        IF(STAT .EQ. GLIST_STAT_GOOD) PRINT *, 'REMOVED BUFFER FROM APPLICATION QUEUE FY =)'
        IF(STAT .EQ. GLIST_STAT_LASTONE) PRINT *, 'LAST BUFFER FROM THE APPLICATION QUEUE GOOD TO GO'

C        CALL LIST_SIZE(QUETAB(1,DIS))
        CALL LISTSIZE(QUETAB(1,DIS),APPQUEUESIZE)
        PRINT *, 'APP QUEUE SIZE:', APPQUEUESIZE        

      END      

      SUBROUTINE RALLBUFFS_FROMAPPQUEUE(LIST,ALLBLOCK)
        IMPLICIT NONE  
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'   
        INCLUDE 'INCLIB:TASKID.DEF'  
        INCLUDE 'INCLIB:QUECOM.DEF'
	      INCLUDE 'INCLIB:GLIST.DEF'                     
C        INCLUDE 'OLMCON.DEF'

        INTEGER*4 OPT,ST, BUFNUM, STAT
	      INTEGER*4 LIST(*)        
C        CHARACTER QUEUE
        INTEGER*4 QUEUE, APPQUEUESIZE /0/
        INTEGER*4 MAX_LENGTH /20/
	      INTEGER*4 I, LIMIT, TOP, USED
        INTEGER*4 IND
        LOGICAL ALLBLOCK

10	CONTINUE    

        TOP = LIST(GLIST_TOP)
C        CALL LIST_SIZE(QUETAB(1,DIS))

        CALL LISTSIZE(QUETAB(1,IGC),USED) !DIS
        PRINT *, 'APP QUEUE SIZE:', USED 
      
        IND = TOP
        DO 20 I = 1, USED
          IND = IND + 1
          IF(IND.GT.LIST( GLIST_MAX_OFFSET))THEN
            IND = GLIST_START
          ENDIF
          PRINT *, 'DISPAT BUFFER POS',I , ' : ' ,LIST(IND)

          CALL RTL (BUFNUM, QUETAB(1,DIS), STAT)
          IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
            BUFNUM = 0
          ENDIF 
          CALL RELBUF(BUFNUM)
          
C          PRINT *, 'REMOVEL STATUS:', STAT
          IF(STAT .EQ. GLIST_STAT_EMPTY) PRINT *, 'APPLICATION QUEUE LIST EMPTY...'
          IF(STAT .EQ. GLIST_STAT_GOOD) PRINT *, 'REMOVED BUFFER FROM APPLICATION QUEUE FY =)'
          IF(STAT .EQ. GLIST_STAT_LASTONE) PRINT *, 'LAST BUFFER FROM THE APPLICATION QUEUE GOOD TO GO'

          IF(ALLBLOCK) THEN
            IF(I.GT.MAX_LENGTH ) GOTO 9000
          ENDIF
20	CONTINUE
C
9000	CONTINUE

C        CALL LIST_SIZE(QUETAB(1,DIS))
        CALL LISTSIZE(QUETAB(1,DIS),USED)
        PRINT *, 'APP QUEUE SIZE:', USED        

      END

      SUBROUTINE LIST_SIZE (LIST_ARRAY)
        IMPLICIT NONE    
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE	'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'
        
        
        INTEGER*4 LIST_ARRAY(:)
        INTEGER*4 APPQUEUESIZE /0/
        
        APPQUEUESIZE = size(LIST_ARRAY)
        PRINT *, 'APP QUEUE SIZE:', APPQUEUESIZE

      END

