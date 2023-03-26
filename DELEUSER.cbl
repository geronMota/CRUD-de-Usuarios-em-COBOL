      ******************************************************************
      * Author:JEFFERSON MOTA SILVA(GERO)
      * Date:26/03/23
      * Purpose:PRATICAR CRUD EM COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELEUSER.

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
       77 WS-EOF                         PIC X.
        88 EOF-OK                        VALUE "S" FALSE "N".
       77 WS-COUNT                       PIC 9.

       77 WS-S-MASTER                           PIC X(6) VALUE "ABC123".
       77 WS-S-GESTOR                           PIC X(6) VALUE "DEF456".
       77 WS-S-ANALISTA                         PIC X(6) VALUE "GHI789".

       77 WS-S-ALTE-USER                        PIC X(6).
       77 WS-CONFIRM                            PIC X.

       01 CAD-USER.
           03 WS-CD-USER                 PIC 9(6).
           03 WS-NM                      PIC X(25).
           03 WS-EMAIL                   PIC X(30).
           03 WS-PHONE                   PIC 9(12).
           03 WS-PASSWORD                PIC X(8).

       PROCEDURE DIVISION.
       P600-DELETAR.
            OPEN I-O CADUSER

             DISPLAY "DIGITE O CODIGO DO REGISTRO A SER DELETADO"
             ACCEPT WS-CD-USER

              MOVE WS-CD-USER TO FD-CD-USER

            IF FS-CADUSER-OK THEN

              READ CADUSER INTO CAD-USER
              KEY IS FD-CD-USER

               INVALID KEY
               DISPLAY "CHAVE NAO ENCONTRADA"
               PERFORM P601-M-DELETAR THRU  P601-FIM

           NOT INVALID KEY
               DISPLAY "REGISTRO:"
                         FD-CD-USER
                DISPLAY "NOME:"
                         FD-NM
                 DISPLAY "EMAIL:"
                         FD-EMAIL
                  DISPLAY "PHONE:"
                         FD-PHONE
                  DISPLAY "PASSWORD:"
                         FD-PASSWORD
            END-READ

            DISPLAY "TEM CERTEZA QUE DESEJA EXCLUIR O REGISTRO?"
            DISPLAY " DIGITE <S> PARA SIM E <N> PARA NAO"
            ACCEPT WS-CONFIRM

            IF WS-CONFIRM EQUAL "S" THEN
                DISPLAY "DIGITE A SENHA PARA EXCLUSAO"
                ACCEPT WS-S-ALTE-USER
                IF WS-S-ALTE-USER EQUAL WS-S-MASTER OR WS-S-ANALISTA
                                                    OR WS-S-GESTOR
                DELETE CADUSER RECORD
                DISPLAY "REGISTRO EXCLUIDO COM SUCESSO"

                ELSE
            IF WS-CONFIRM EQUAL "N" THEN
                DISPLAY "MISSAO ABORTADA"
            END-IF
            CLOSE CADUSER.
       P601-M-DELETAR.
             DISPLAY "<1> PARA NOVA EXCLUSAO"
             DISPLAY "<M> PARA VOLTAR AO MENU"
             DISPLAY "<F> PARA FINALIZAR"

               ACCEPT WS-OPCAO.

               IF WS-OPCAO EQUAL "F" THEN
               GO TO FINALIZAR
               ELSE
               IF WS-OPCAO EQUAL 1 THEN
               GO TO P600-DELETAR.
       P601-FIM.
       P600-FIM.
           FINALIZAR.
            STOP RUN.
       END PROGRAM DELEUSER.
