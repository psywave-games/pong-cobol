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

       78 C-BLACK              VALUE 0.
      *----------------------------------------------------------------*
      *    PLAYER-VARIABLES
      *----------------------------------------------------------------*
      *    P: PLAYER
       78 P-WIDTH              VALUE 8.
       78 P-HEIGHT             VALUE 80.
       78 P-POSX               VALUE 10.
       78 P-SPEED              VALUE 16.
       77 P-POSY   PIC 999V99.
      *----------------------------------------------------------------*
      *    BALL-VARIABLES
      *----------------------------------------------------------------*
      *    B: BALL
       78 B-SIZE               VALUE 16.
       77 B-POSX   PIC 9(3)V9  VALUE 780.
       77 B-POSY   PIC 9(3)V9  VALUE 225.
       77 B-HSPEED PIC S9(2)V9 VALUE -5.0.
       77 B-VSPEED PIC S9(2)V9 VALUE ZERO.
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
           CALL "SetTargetFPS" USING BY VALUE 30
                   RETURNING OMITTED
           END-CALL.
      *----------------------------------------------------------------*
       GAME-INIT                                                SECTION.
           PERFORM BALL-RANDOM.
      *----------------------------------------------------------------*
       GAME-LOOP                                                SECTION.
           PERFORM UNTIL K-ESC = 1
               CALL "WindowShouldClose"
                   RETURNING K-ESC
               END-CALL
            
               PERFORM GAME-INPUT
               PERFORM PLAYER-MOVE
               PERFORM BALL-COLISION
               PERFORM BALL-MOVE
               PERFORM GAME-DRAW

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

           CALL "ClearBackground" USING BY VALUE C-BLACK
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
           IF R-KEY-DOWN = K-PRESSED 
               AND SUM(P-POSY, P-HEIGHT, 1) < W-HEIGHT THEN 
                   ADD P-SPEED TO P-POSY
           ELSE
               IF R-KEY-UP = K-PRESSED AND P-POSY > 1 THEN
                   SUBTRACT P-SPEED FROM P-POSY
           END-IF. 
      *----------------------------------------------------------------*
       PLAYER-DRAW                                              SECTION.
           CALL static "DrawRectangle" USING
               BY VALUE P-POSX P-POSY
               BY VALUE P-WIDTH P-HEIGHT
               BY CONTENT C-WHITE
           END-CALL.
      *----------------------------------------------------------------*
       BALL-RANDOM                                              SECTION.
           PERFORM WITH TEST AFTER UNTIL ABS (B-VSPEED) > 4
               CALL "GetRandomValue" USING
                   BY VALUE -7
                   BY VALUE 7
                   RETURNING B-VSPEED
               END-CALL
           END-PERFORM.
      *----------------------------------------------------------------*
       BALL-MOVE                                                SECTION.
           ADD B-HSPEED TO B-POSX 
           ADD B-VSPEED TO B-POSY
           IF B-POSY <= 1 THEN
               PERFORM GAME-END
           END-IF.
      *----------------------------------------------------------------*
       BALL-COLISION                                            SECTION.
           IF B-POSY <= B-SIZE/2
               OR B-POSY >= W-HEIGHT - B-SIZE/2 THEN 
               MULTIPLY -1 BY B-VSPEED
           END-IF
           IF B-POSX >= W-WIDTH - B-SIZE/2 THEN
               MULTIPLY -1 BY B-HSPEED
               PERFORM BALL-RANDOM 
           END-IF
           IF B-POSX <= P-WIDTH
               AND B-POSY > P-POSY
               AND B-POSY < P-POSY + P-HEIGHT THEN
               MULTIPLY -1.2 BY B-HSPEED
               PERFORM BALL-RANDOM 
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
