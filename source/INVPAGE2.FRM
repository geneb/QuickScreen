" e                                                           G   	              STOCK9                                  P  /               DESCR9                                  o 1 3               UM9                                     r 5 <               QUANT9         $�j�����$�j����G        v > E  
             UNITP9                 �������        ~ G O  
             AMOUNT9    ��  ���������������        �   	              STOCK10                                 �  /               DESCR10                                 � 1 3               UM10                                    � 5 <               QUANT10        $�j�����$�j����G        � > E  
             UNITP10                �������        � G O  
             AMOUNT10   ��  ���������������        �   	              STOCK11                                 �  /               DESCR11                                 � 1 3               UM11                                    � 5 <               QUANT11        $�j�����$�j����G        � > E  
             UNITP11                �������        � G O  
             AMOUNT11   ��  ���������������           	              STOCK12                                   /               DESCR12                                 , 1 3               UM12                                    / 5 <               QUANT12        $�j�����$�j����G        3 > E  
             UNITP12                �������        ; G O  
             AMOUNT12   ��  ���������������        C                 TAXRATE                $�j����G        G - -     �         EXEMPT                                  H E O  
             SUBTOTAL   ��  ���������������        P E O  
             SALESTAX   ��          �������$       X	 E O  
             TOTALDUE   ��  ���������������        `  N               NOTES                                  c                   SAVE2         ��                        c   #                GETREC2       ��                        d  ) <                NEXTPG2       ��                        e  B O                QUIT2                                  C




QUANT9*UNITP9





QUANT10*UNITP10





QUANT11*UNITP11





QUANT12*UNITP12


AMOUNT1+AMOUNT2+AMOUNT3+AMOUNT4+AMOUNT5+AMOUNT6+AMOUNT7+AMOUNT8+AMOUNT9+AMOUNT10+AMOUNT11+AMOUNT12
TAXRATE / 100 * SUBTOTAL * ABS(BILLSTAT = "CT" AND EXEMPT = " ")
SUBTOTAL+SALESTAX





HOur stock number for the item.
Description of the item.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF"
Quantity or number of items.
Unit price or cost per item.

Our stock number for the item.
Description of the item.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF"
Quantity or number of items.
Unit price or cost per item.

Our stock number for the item.
Description of the item.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF"
Quantity or number of items.
Unit price or cost per item.

Our stock number for the item.
Description of the item.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF"
Quantity or number of items. 
Unit price or cost per item.

Our states sales tax rate as percent.
Check if buyer has a state tax number.  Use the <Space Bar> to toggle the checkmark on and off.



Enter any sales notes or special shipping instructions. 
Press this button to save the data on the form to disk. 
Press this button to retrieve previously saved data. 
Press this button to go to the previous page of the form. 
Press this key to cancel the data on the form and quit. 
