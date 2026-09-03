       IDENTIFICATION DIVISION .
           PROGRAM-ID. IMC.
           AUTHOR. ROMULO CESAR.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NOME         PIC A(020).
       01 ALTURA       PIC 9V99.
       01 PESO         PIC 999.
       01 IMC_TOTAL    PIC ZZ9.99.

       PROCEDURE DIVISION.
       PRINCIPAL.
           DISPLAY "Entre com seu Nome: ".
           ACCEPT NOME.
           DISPLAY "Por favor " NOME " Digite a sua altura (Em MT): ".
           ACCEPT ALTURA.
           DISPLAY "Agora Digite o seu peso (Em KG): ".
           ACCEPT PESO.
           DISPLAY "Sua Altura é: " ALTURA " Seu peso é: " PESO.

           COMPUTE IMC_TOTAL = PESO / (ALTURA ** 2).
           DISPLAY "Seu IMC é " IMC_TOTAL.
           

       STOP RUN.
       END PROGRAM IMC.  
       