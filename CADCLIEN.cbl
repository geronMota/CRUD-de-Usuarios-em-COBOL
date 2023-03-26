      ******************************************************************
      * Author:JEFFERSON MOTA SILVA(GERO)
      * Date:26/03/23
      * Purpose:PRATICAR CRUD EM COBOL/CADASTRO DE USUARIO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCLIEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADUSER ASSIGN TO
           "C:\Users\PC\Desktop\CRUD\CADUSER.cbl"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FD-CD-USER
           FILE STATUS IS WS-FS-CADUSER.

       DATA DIVISION.
       FILE SECTION.
       FD CADUSER.
       01 REG-USER.
           03 FD-CD-USER                 PIC 9(6).
           03 FD-NM                      PIC X(25).
           03 FD-EMAIL                   PIC X(30).
           03 FD-PHONE                   PIC 9(12).
           03 FD-PASSWORD                PIC X(8).

       WORKING-STORAGE SECTION.
       77 WS-FS-CADUSER                  PIC 99.
        88 FS-CADUSER-OK                 VALUE 0.
       77 WS-OPCAO                       PIC X.
       01 CAD-USER.
           03 WS-CD-USER                 PIC 9(6).
           03 WS-NM                      PIC X(25).
           03 WS-EMAIL                   PIC X(30).
           03 WS-PHONE                   PIC 9(12).
           03 WS-PASSWORD                PIC X(8).

       PROCEDURE DIVISION.

           P200-CADASTRAR.

            OPEN I-O CADUSER

           DISPLAY "==================================================="
           DISPLAY "*               CADASTRO DE USUARIOS              *"
           DISPLAY "==================================================="
           DISPLAY "DIGITE O REGISTRO"
           ACCEPT WS-CD-USER
           DISPLAY "==================================================="
           DISPLAY "CADASTRE O NOME DO USUARIO"
           ACCEPT WS-NM
           DISPLAY "==================================================="
           DISPLAY "DIGITE O EMAIL DO USUARIO"
           ACCEPT WS-EMAIL
           DISPLAY "==================================================="
           DISPLAY "DIGITE O PHONE DO USUARIO"
           ACCEPT WS-PHONE
           DISPLAY "==================================================="
           DISPLAY "DIGITE O PASSWORD"
           ACCEPT WS-PASSWORD
           DISPLAY "==================================================="
           .

           IF WS-FS-CADUSER EQUAL 35 THEN
               OPEN OUTPUT CADUSER
           END-IF.

           IF FS-CADUSER-OK THEN

               MOVE WS-CD-USER           TO FD-CD-USER
               MOVE WS-NM                TO FD-NM
               MOVE WS-EMAIL             TO FD-EMAIL
               MOVE WS-PHONE             TO FD-PHONE
               MOVE WS-PASSWORD          TO FD-PASSWORD



               WRITE REG-USER
               INVALID KEY
           DISPLAY "INVALID KEY,NAO FOI POSSIVEL CADASTRAR "
           WS-FS-CADUSER
           NOT INVALID KEY
               DISPLAY" CADASTRADO COM SUCESSO"
           END-WRITE
           END-IF
           CLOSE CADUSER.

           DISPLAY "<F> PARA FINALIZAR"
           DISPLAY "<1> PARA NOVO CADASTRO"
           ACCEPT WS-OPCAO.

               IF WS-OPCAO EQUAL "F" THEN
                   GO TO FINALIZAR
           ELSE
               IF WS-OPCAO EQUAL 1 THEN
                   GO TO P200-CADASTRAR.

       P200-FIM.

           FINALIZAR.
            STOP RUN.
       END PROGRAM CADCLIEN.
