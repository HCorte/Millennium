C
C SUBROUTINE VLOAD
C $Log:   GXAFXT:[GOLS]VLOAD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:55:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   18 Jun 1993 20:15:08   HXK
C  Initial revision.
C
C ** Source - vsystap.fcc **
C
                                                                            
C--------------------------------------------------                             
                                                                                
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
      SUBROUTINE VLOAD                                                          
      IMPLICIT NONE
                                                                                
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:LSYSCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'

      INTEGER*4  MAX_STR                                                     
      PARAMETER (MAX_STR  =24)  !max string length                              
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
      CHARACTER*20 TAB_FILENAME                                                 
      CHARACTER*24 VSTRING !should be MAX_STR, contains line data in table file.
                                                                                
      CHARACTER  BELL         /Z07/                                             
      INTEGER*4  TAB_FILEU    /6/,!file unit for table files.                   
     *           CLU          /5/,!console unit                                 
     *           VIOSTAT,                                                       
     *           VGAME_NBR,       !game #                                       
     *           VSYS_NBR,        !syst #                                       
     *           VNB_MARKS,       !nb of system marks, eg 12,13...
     *           VNB_BOARDS,      !nb of boards      , eg 41,66...
     *           VVALID_MARKS,    !nb of valid marks , eg 6 for Viking 6/48
     *           VI4_WORD                                                       
                                                                                
      INTEGER*4  POINTER,I


                                                                                
                                                                                
      TYPE*,'VLOAD CALLED'                                                      
                                                                                
                                                                                
                                                                                
C Read reduced tables from file                                                 
C -----------------------------                                                 
      CALL WIMG(CLU,'Enter table file name : ')                                 
      READ(5,903) TAB_FILENAME                                                  
      TYPE *,'Table file name : ***',TAB_FILENAME,'***'                         
                                                                                
      OPEN (UNIT    =TAB_FILEU,                                                 
     *      IOSTAT  =VIOSTAT,                                                   
     *      STATUS  ='OLD',                                                     
     *      FILE    =TAB_FILENAME,                                              
     *      ACCESS  ='SEQUENTIAL'                                               
     *     )                                                                    
                                                                                
      IF (VIOSTAT.NE.0) THEN                                                    
         TYPE*,' Error when open file. Status = ',VIOSTAT                       
         CLOSE (UNIT=TAB_FILEU)                                                 
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
C     Read 1rst line                                                            
C     --------------                                                            
      READ (UNIT=TAB_FILEU,FMT=9000,IOSTAT=VIOSTAT)                             
     *     VGAME_NBR,VSYS_NBR,VNB_MARKS,VNB_BOARDS,VVALID_MARKS
                                                                                
      IF (VIOSTAT.NE.0) THEN                                                    
         TYPE*,' Error when read file. Status = ',VIOSTAT                       
         CLOSE (UNIT=TAB_FILEU)                                                 
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
      WRITE (CLU,9200) VGAME_NBR,VSYS_NBR,VNB_MARKS,VNB_BOARDS,VVALID_MARKS
                                                                                
      IF (LSYS_GAME(VSYS_NBR).GT.0) THEN                                        
          TYPE*,'Reduced bets already loaded for this Syst # ',VSYS_NBR         
          TYPE*,'Actual LSYS_GAME(VSYS_NBR) = ',LSYS_GAME(VSYS_NBR)             
          TYPE*,BELL,BELL                                                       
          PAUSE                                                                 
      ENDIF                                                                     
                                                                                
                                                                                
C***  LSYS_FREEPTR=0         !set system pointer to zero ....               
      POINTER=LSYS_FREEPTR+1 !set system pointer to the next available position
                             !in the LSYS_TAB table.

C Loop reading all records (lines) from table file
C ------------------------------------------------
      DO 3000 I=1,VNB_BOARDS                                                    
         READ (UNIT=TAB_FILEU,FMT=9010,IOSTAT=VIOSTAT)                       
     *         VSTRING                                                        
                                                                                
         VI4_WORD=0            !reset it                                     
         CALL FILL_I4WORD (VSTRING,VNB_MARKS,VI4_WORD)                       
         CALL VSYSRED     (VI4_WORD,VNB_MARKS,VSYS_NBR,VVALID_MARKS,I,POINTER) 
C                          POINTER is modified by VSYSRED !!!
 3000 CONTINUE                                                                  
                                                                                
C           Fill common ...                                                     
C           -----------                                                         
            LSYS_ATR   (VSYS_NBR)=LSYS_REDUCED                                  
            LSYS_NUMBET(VSYS_NBR)=VNB_BOARDS                                    
            LSYS_GAME  (VSYS_NBR)=VGAME_NBR                                     
            LSYS_NUMMRK(VSYS_NBR)=VNB_MARKS                                     
            LSYS_PTR   (VSYS_NBR)=LSYS_FREEPTR+1 !start of reduced sys table.
            LSYS_BOARD (VSYS_NBR)=VNB_BOARDS                                    
            LSYS_FREEPTR         =POINTER-1      !last occupied position in LSYS_TAB
                                                                                
C           WRITE (CLU,9020) I,VSTRING,VI4_WORD                                 
C           TYPE*,' pointer ',POINTER,' system number ',VSYS_NBR                
                                                                                
C     PAGE=0                                                                    
C     COPY=1   !pre set for now...                                              
C     CALL SPOOL(CXREPNAM,COPY,ST)                                              
C     TYPE*,REP_NAME,' has been spooled.',BELL                                  
      TYPE*,' Please check report prior to saving image to file.',BELL          
                                                                                
      CLOSE (UNIT=TAB_FILEU)                                                    
      RETURN                                                                    
                                                                                
                                                                                
 903  FORMAT(20A)                                                               
 9000 FORMAT(5I)                                                                
 9010 FORMAT(24A)                                                               
C9020 FORMAT(1X,I4,' ',A30,'    ',Z8)                                           
 9200 FORMAT(1X,' Game #            : ',I4,/,
     *       1X,' Syst #            : ',I4,/,
     *       1X,' Nb of marks       : ',I4,/,
     *       1X,' Nb of boards      : ',I4,/,
     *       1X,' Nb of valid marks : ',I4)
      END                                  
