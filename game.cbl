      *================================================================* 
       IDENTIFICATION                                          DIVISION.
      *================================================================*
      *    Compile with param: cobc -xjd game.cbl -lraylib
       PROGRAM-ID.         PONG0001.

       AUTHOR.             RODRIGO DORNELLES.
       INSTALLATION.       PSYWAVE GAMES.

       DATE-WRITTEN.       31/01/2021.
       DATE-COMPILED.      31/01/2021.
      *================================================================*        
       ENVIRONMENT                                             DIVISION.
      *================================================================*
       CONFIGURATION                                            SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
      *================================================================*
       DATA                                                    DIVISION.
      *================================================================*
       WORKING-STORAGE                                          SECTION.
      *----------------------------------------------------------------*
      *    GAME-VARIABLES
      *----------------------------------------------------------------*
      *    R: RETURN
      *    W: WINDOW
      *    K: KEY
      *    C: COLOR
   
       01 R-CODE USAGE BINARY-LONG.
       
       01 K-ESC PIC 9(8).
       
       78 W-WIDTH      VALUE 800.
       78 W-HEIGHT     VALUE 450.
       78 W-NAME       VALUE "PONG COBOL GAME".

       01 C-WHITE.
           02 R PIC S9(3) VALUE 245 BINARY.
           02 G PIC S9(3) VALUE 245 BINARY.
           02 B PIC S9(3) VALUE 245 BINARY.
           02 A PIC S9(3) VALUE 255 BINARY.

       01 C-BLACK.
           02 R PIC S9(3) VALUE 0 BINARY.
           02 G PIC S9(3) VALUE 0 BINARY.
           02 B PIC S9(3) VALUE 0 BINARY.
           02 A PIC S9(3) VALUE 255 BINARY.
      *----------------------------------------------------------------*
      *    PLAYER-VARIABLES
      *----------------------------------------------------------------*
      *    P: PLAYER
       78 P-WIDTH         VALUE 8.
       78 P-HEIGHT        VALUE 80.
       77 P-POSY PIC 999  VALUE 225.
      *----------------------------------------------------------------*
      *    BALL-VARIABLES
      *----------------------------------------------------------------*
      *    B: BALL
       78 B-SIZE         VALUE 16.
       77 B-POSX PIC 999.
       77 B-POSY PIC 999.
       77 B-HSPEED PIC S9(3).
       77 B-VSPEED PIC S9(3).
      *================================================================*
       PROCEDURE                                               DIVISION.
      *================================================================*
       MAIN-PROCEDURE.
       PERFORM INIT-WINDOW.
       PERFORM GAME-LOOP.
       PERFORM CLOSE-WINDOW.
       GOBACK.
      *----------------------------------------------------------------*
       INIT-WINDOW                                              SECTION.
           CALL "InitWindow" USING
               BY VALUE W-WIDTH W-HEIGHT
               BY REFERENCE W-NAME RETURNING R-CODE
                   ON EXCEPTION
                   DISPLAY "exception error: raylib not found"
                   UPON SYSERR
                   END-DISPLAY
           END-CALL
           CALL "SetTargetFPS" USING BY VALUE 0
                   RETURNING OMITTED
           END-CALL.
      *----------------------------------------------------------------*
       GAME-LOOP                                                SECTION.
           PERFORM UNTIL K-ESC = 1
               CALL "WindowShouldClose"
                   RETURNING K-ESC
               END-CALL
            
               PERFORM GAME-DRAW
               
           END-PERFORM.
      *----------------------------------------------------------------*
       GAME-DRAW                                                SECTION.
           CALL STATIC "BeginDrawing"
               RETURNING OMITTED
           END-CALL 

           CALL "ClearBackground" USING BY REFERENCE C-BLACK
               RETURNING OMITTED
           END-CALL

           CALL STATIC "EndDrawing"
               RETURNING OMITTED
           END-CALL.
      *----------------------------------------------------------------*
       CLOSE-WINDOW                                             SECTION.
           CALL "CloseWindow"
               RETURNING OMITTED
           END-CALL.