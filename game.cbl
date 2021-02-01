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
       01 R-DELTATIME PIC S9(1)V9(8).
       01 R-KEY-UP    PIC 9.
       01 R-KEY-DOWN  PIC 9.
      
       01 K-UP     PIC 9(8)    VALUE 265.
       01 K-DOWN   PIC 9(9)    VALUE 264.
       01 K-ESC    PIC 9(8)    VALUE 256.
       78 K-PRESSED            VALUE 7.
       
       78 W-WIDTH              VALUE 800.
       78 W-HEIGHT             VALUE 450.
       78 W-NAME               VALUE "PONG COBOL GAME".

       01 C-WHITE.
           02 R    PIC S9(3)   VALUE 245 BINARY.
           02 G    PIC S9(3)   VALUE 245 BINARY.
           02 B    PIC S9(3)   VALUE 245 BINARY.
           02 A    PIC S9(3)   VALUE 255 BINARY.

       01 C-BLACK.
           02 R    PIC S9(3)   VALUE 0 BINARY.
           02 G    PIC S9(3)   VALUE 0 BINARY.
           02 B    PIC S9(3)   VALUE 0 BINARY.
           02 A    PIC S9(3)   VALUE 255 BINARY.
      *----------------------------------------------------------------*
      *    PLAYER-VARIABLES
      *----------------------------------------------------------------*
      *    P: PLAYER
       78 P-WIDTH              VALUE 8.
       78 P-HEIGHT             VALUE 80.
       78 P-POSX               VALUE 10.
       77 P-POSY   PIC 9(3)V9(8).
      *----------------------------------------------------------------*
      *    BALL-VARIABLES
      *----------------------------------------------------------------*
      *    B: BALL
       78 B-SIZE               VALUE 16.
       77 B-POSX   PIC 9(3)V9  VALUE 780.
       77 B-POSY   PIC 9(3)V9  VALUE 225.
       77 B-HSPEED PIC SV9     VALUE -0.6.
       77 B-VSPEED PIC SV9     VALUE ZERO.
      *================================================================*
       PROCEDURE                                               DIVISION.
      *================================================================*
       MAIN-PROCEDURE.
       PERFORM INIT-WINDOW.
       PERFORM GAME-INIT.
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
       GAME-INIT                                                SECTION.
      *----------------------------------------------------------------*
       GAME-LOOP                                                SECTION.
           PERFORM UNTIL K-ESC = 1
               CALL "WindowShouldClose"
                   RETURNING K-ESC
               END-CALL
            
               PERFORM GAME-INPUT
               PERFORM PLAYER-MOVE
               PERFORM BALL-MOVE
               PERFORM GAME-DRAW

               CALL "GetFrameTime" 
                   RETURNING R-DELTATIME
               END-CALL
               
           END-PERFORM.
      *----------------------------------------------------------------*
       GAME-INPUT                                               SECTION.
           CALL "IsKeyDown" USING 
               BY VALUE K-UP
               RETURNING R-KEY-UP
           END-CALL

           CALL "IsKeyDown" USING 
               BY VALUE K-DOWN
               RETURNING R-KEY-DOWN
           END-CALL.
      *----------------------------------------------------------------*
       GAME-DRAW                                                SECTION.
           CALL STATIC "BeginDrawing"
               RETURNING OMITTED
           END-CALL 

           CALL "ClearBackground" USING BY REFERENCE C-BLACK
               RETURNING OMITTED
           END-CALL

           PERFORM PLAYER-DRAW
           PERFORM BALL-DRAW

           CALL STATIC "EndDrawing"
               RETURNING OMITTED
           END-CALL.
      *----------------------------------------------------------------*
       GAME-END                                                 SECTION.
      *----------------------------------------------------------------*
       PLAYER-MOVE                                              SECTION.
           IF R-KEY-UP = K-PRESSED THEN 
               SUBTRACT 1 FROM P-POSY
           END-IF
           IF R-KEY-DOWN = K-PRESSED THEN 
               IF SUM(P-POSY, P-HEIGHT) < W-HEIGHT THEN
                   ADD 1 TO P-POSY
               END-IF
           END-IF.
      *----------------------------------------------------------------*
       PLAYER-DRAW                                              SECTION.
           CALL static "DrawRectangle" USING
               BY VALUE P-POSX P-POSY
               BY VALUE P-WIDTH P-HEIGHT
               BY CONTENT C-WHITE
           END-CALL.
      *----------------------------------------------------------------*
       BALL-MOVE                                                SECTION.
           ADD B-HSPEED TO B-POSX
           ADD B-VSPEED TO B-POSY
           IF B-POSY <= 1 then
               PERFORM GAME-END
           END-IF.
      *----------------------------------------------------------------*
       BALL-DRAW                                                SECTION.
           CALL static "DrawRectangle" USING
               BY VALUE B-POSX B-POSY B-SIZE B-SIZE
               BY CONTENT C-WHITE
           END-CALL.
      
      *----------------------------------------------------------------*
       CLOSE-WINDOW                                             SECTION.
           CALL "CloseWindow"
               RETURNING OMITTED
           END-CALL.
