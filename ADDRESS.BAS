OPTION BASE 0
DEFINT A-Z

TYPE SelectionStr
 Column AS INTEGER
 Page AS INTEGER
 Row AS INTEGER
END TYPE

DECLARE FUNCTION FileExists (SearchFileName AS STRING)
DECLARE FUNCTION GetChoice$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, Choices AS STRING)
DECLARE FUNCTION GetInput$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, MaximumLength AS INTEGER, IsFileDialog AS INTEGER, AllowSpecialCharacters AS INTEGER)
DECLARE FUNCTION GetSpecialCharacter$ ()
DECLARE FUNCTION LoadFile$ (AddressData() AS STRING)
DECLARE FUNCTION PrintDialog (Selection AS SelectionStr, FirstPage AS INTEGER, LastPage AS INTEGER)
DECLARE FUNCTION SaveFileAs$ (AddressData() AS STRING)
DECLARE FUNCTION SearchData (AddressData() AS STRING, SearchText AS STRING, SearchFile AS STRING, SearchFileH AS INTEGER, TextFound AS INTEGER)
DECLARE FUNCTION SearchDialog (SearchText AS STRING, SearchFile AS STRING, SearchResult AS SelectionStr)
DECLARE SUB AddFileNameToList (FileName AS STRING)
DECLARE SUB Center (x AS INTEGER, Text AS STRING)
DECLARE SUB DeleteFiles ()
DECLARE SUB DisplayFileList ()
DECLARE SUB DisplayHelp ()
DECLARE SUB DisplayInterface ()
DECLARE SUB DisplaySpecialCharacters ()
DECLARE SUB DisplayStatusBar (Selection AS SelectionStr, CurrentFileName AS STRING)
DECLARE SUB DisplayTable (AddressData() AS STRING, Page AS INTEGER)
DECLARE SUB DisplayTableItem (AddressData() AS STRING, Selection AS SelectionStr, Highlight AS INTEGER)
DECLARE SUB DisplayTitle ()
DECLARE SUB DrawBox (x AS INTEGER, y AS INTEGER, BoxWidth AS INTEGER, BoxHeight AS INTEGER, HasShadow AS INTEGER, Title AS STRING)
DECLARE SUB Initialize (Selection AS SelectionStr, CurrentFileName AS STRING)
DECLARE SUB InitializeFileList ()
DECLARE SUB Main ()
DECLARE SUB PrintTable (AddressData() AS STRING, Selection AS SelectionStr)
DECLARE SUB Quit ()
DECLARE SUB RemoveFile ()
DECLARE SUB SaveFile (AddressData() AS STRING, FileName AS STRING)
DECLARE SUB Search (AddressData() AS STRING, FileName AS STRING)
ON ERROR GOTO ErrorTrap

DisplayTitle
CALL Main

ErrorTrap:
DIM ErrorCode AS INTEGER
DIM KeyStroke AS STRING

 ErrorCode = ERR
 PCOPY 0, 3
 COLOR 0, 7
 SELECT CASE ErrorCode
  CASE 7, 14
   Center 5, " Not enough memory. "
  CASE 27
   Center 5, " No paper in printer. "
  CASE 52, 64
   Center 5, " Bad file name. "
  CASE 53
   Center 5, " Cannot find file. "
  CASE 61
   Center 5, " Disk is full. "
  CASE 62
   Center 5, " Error while   "
   Center 0, " reading file. "
  CASE 70
   Center 5, " Disk is write- "
   Center 0, " protected.     "
  CASE 71
   Center 5, " No disk in drive. "
  CASE 72
   Center 5, " Disk is damaged. "
  CASE 75, 76
   Center 5, " Cannot find "
   Center 0, " directory.  "
  CASE ELSE
   Center 5, " Unexpected error. "
 END SELECT

 COLOR 7, 0
 Center 0, " Spacebar = Ignore  Enter = Retry  Escape = Quit "
 DO
  KeyStroke = INPUT$(1)
  PCOPY 3, 0
  SELECT CASE KeyStroke
   CASE " "
    RESUME NEXT
   CASE CHR$(13)
    RESUME
   CASE CHR$(27)
    Quit
  END SELECT
 LOOP

SUB AddFileNameToList (FileName AS STRING)
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN "Names.lst" FOR APPEND LOCK READ WRITE AS FileH
  PRINT #FileH, UCASE$(FileName)
 CLOSE FileH
END SUB

SUB Center (x AS INTEGER, Text AS STRING)
 IF x > 0 THEN LOCATE x
 LOCATE , INT(40 - (LEN(Text) / 2))
 PRINT Text
END SUB

SUB DeleteFiles
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN "Names.lst" FOR OUTPUT LOCK READ WRITE AS FileH
 CLOSE FileH

 KILL "*.adr"
END SUB

SUB DisplayFileList
DIM FileH AS INTEGER
DIM FileName AS STRING
DIM Item AS INTEGER
DIM KeyStroke AS STRING

 PCOPY 0, 2
 COLOR 7, 0
 CLS

 FileH = FREEFILE
 OPEN "Names.lst" FOR INPUT LOCK READ WRITE AS FileH
  IF LOF(FileH) > 0 THEN
   DO UNTIL EOF(FileH) OR KeyStroke = CHR$(27)
    COLOR 7, 0
    CLS

    FOR Item = 0 TO 175
     IF EOF(FileH) THEN EXIT FOR
     LINE INPUT #FileH, FileName
     PRINT USING " \      \ "; FileName;
    NEXT Item

    COLOR 0, 7
    LOCATE 23, 3
    PRINT "Escape = Cancel   Any other key = Continue"
    KeyStroke = INPUT$(1)
   LOOP
  ELSE
   PRINT "There are no saved files. Press any key to continue..."
   KeyStroke = INPUT$(1)
  END IF
 CLOSE FileH

 PCOPY 2, 0
END SUB

SUB DisplayHelp
DIM FileH AS INTEGER
DIM KeyStroke AS STRING
DIM Text AS STRING

 PCOPY 0, 1
 COLOR 0, 7
 DrawBox 3, 14, 50, 15, -1, "Help"

 FileH = FREEFILE
 OPEN "Address.hlp" FOR INPUT LOCK READ WRITE AS FileH
  DO UNTIL EOF(FileH)
   LINE INPUT #FileH, Text
   LOCATE , 20
   PRINT Text
  LOOP
 CLOSE FileH

 KeyStroke = INPUT$(1)

 PCOPY 1, 0
END SUB

SUB DisplayInterface
 COLOR 0, 7
 LOCATE 1, 1
 PRINT " Name:           Address:        City:           Postal Code:    Phone Number:  ";

 COLOR 7, 0
 LOCATE 22, 1
 PRINT STRING$(80, "Ä")
 LOCATE 23, 60
 PRINT " F1 = Help "
END SUB

SUB DisplaySpecialCharacters
DIM CharacterCode AS INTEGER
DIM Column AS INTEGER
DIM Row AS INTEGER

 CharacterCode = 127
 FOR Row = 0 TO 7
  FOR Column = 0 TO 15
   LOCATE Row + 6, (Column * 2) + 23
   PRINT CHR$(CharacterCode);
   CharacterCode = CharacterCode + 1
  NEXT Column
 NEXT Row
END SUB

SUB DisplayStatusBar (Selection AS SelectionStr, CurrentFileName AS STRING)
 COLOR 7, 0
 LOCATE 23, 3
 PRINT USING " Row: ###  Column: ###  Page: ###  Name: "; Selection.Row + 1; Selection.Column + 1; Selection.Page + 1;
 IF CurrentFileName = "" THEN
  PRINT "no name"
 ELSE
  PRINT USING "\        \"; CurrentFileName;
 END IF
END SUB

SUB DisplayTable (AddressData() AS STRING, Page AS INTEGER)
DIM Column AS INTEGER
DIM Item AS SelectionStr
DIM Row AS INTEGER

 COLOR 7, 0
 Item.Page = Page
 FOR Column = LBOUND(AddressData, 2) TO UBOUND(AddressData, 2)
  FOR Row = LBOUND(AddressData, 1) TO UBOUND(AddressData, 1)
   Item.Column = Column
   Item.Row = Row
   DisplayTableItem AddressData(), Item, 0
  NEXT Row
 NEXT Column
END SUB

SUB DisplayTableItem (AddressData() AS STRING, Selection AS SelectionStr, Highlight AS INTEGER)
 IF Highlight THEN
  COLOR 0, 7
 ELSE
  COLOR 7, 0
 END IF

 LOCATE Selection.Row + 2, (Selection.Column * 16) + 1
 PRINT USING " \            \ "; AddressData(Selection.Row, Selection.Column, Selection.Page);
END SUB

SUB DisplayTitle
DIM FileH AS INTEGER
DIM KeyStroke AS STRING
DIM Row AS STRING

 COLOR 7, 0
 CLS
 LOCATE 2

 FileH = FREEFILE
 OPEN "Title.dat" FOR INPUT LOCK READ WRITE AS FileH
  DO UNTIL EOF(FileH)
   IF FREEFILE <= FileH THEN EXIT DO
   LINE INPUT #FileH, Row
   Center 0, Row$ + SPACE$(70 - LEN(Row))
  LOOP
 CLOSE FileH

 COLOR 0, 7: Center 8, " v1.01 - By: Peter Swinkels, ***1996*** "
 COLOR 7, 0: Center 10, "[ Press any key to continue... ]"

 KeyStroke = INPUT$(1)
END SUB

SUB DrawBox (x AS INTEGER, y AS INTEGER, BoxWidth AS INTEGER, BoxHeight AS INTEGER, HasShadow AS INTEGER, Title AS STRING)
DIM Row AS INTEGER

 LOCATE x, y
 PRINT "Ú"; STRING$(BoxWidth, "Ä"); "¿"

 FOR Row = 1 TO BoxHeight
  LOCATE , y: PRINT "³"; SPC(BoxWidth); "³";
  IF HasShadow THEN PRINT "°°" ELSE PRINT
 NEXT Row

 LOCATE , y
 PRINT "À"; STRING$(BoxWidth, "Ä"); "Ù";

 IF HasShadow THEN
  PRINT "°°"
  LOCATE , y + 2
  PRINT STRING$(BoxWidth + 2, "°");
 END IF

 IF NOT Title = "" THEN Center x, "´ " + Title + " Ã"
END SUB

FUNCTION FileExists (SearchFileName AS STRING)
DIM Exists AS INTEGER
DIM FileH AS INTEGER
DIM FileName AS STRING

 Exists = 0
 FileH = FREEFILE
 OPEN "Names.lst" FOR INPUT LOCK READ WRITE AS FileH
  DO UNTIL EOF(FileH)
   LINE INPUT #FileH, FileName
   IF FileName = UCASE$(SearchFileName) THEN
    Exists = -1
    EXIT DO
   END IF
  LOOP
 CLOSE FileH

 FileExists = Exists
END FUNCTION

FUNCTION GetChoice$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, Choices AS STRING)
DIM Choice AS STRING

 COLOR 7, 0
 LOCATE x, y
 PRINT Prompt;

 DO
  Choice = UCASE$(INPUT$(1))
 LOOP UNTIL INSTR(UCASE$(Choices$), Choice$) > 0

 LOCATE x, y
 PRINT SPACE$(LEN(Prompt));

 GetChoice$ = Choice
END FUNCTION

FUNCTION GetInput$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, MaximumLength AS INTEGER, IsFileDialog AS INTEGER, AllowSpecialCharacters AS INTEGER)
DIM Filter AS STRING
DIM KeyStroke AS STRING
DIM Length AS INTEGER
DIM UserInput AS STRING
 
 Filter = ""
 IF IsFileDialog THEN
  Filter = CHR$(34) + " *+,./<=>?[\]"

  COLOR 7, 0
  LOCATE 23, 1
  PRINT SPACE$(80);

  COLOR 0, 7
  LOCATE 23, 3
  PRINT " F6 = File list "
 END IF

 DO
  COLOR 7, 0
  LOCATE x, y
  PRINT Prompt; UserInput;
  COLOR 23
  PRINT "_ ";

  DO
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
  SELECT CASE KeyStroke
   CASE CHR$(8)
    IF Length > 0 THEN UserInput = LEFT$(UserInput, Length - 1)
   CASE CHR$(13)
    EXIT DO
   CASE CHR$(27)
    UserInput = ""
    EXIT DO
   CASE CHR$(0) + "@"
    IF IsFileDialog THEN DisplayFileList
   CASE CHR$(0) + "B"
    IF AllowSpecialCharacters THEN
     IF Length < MaximumLength THEN
      UserInput = UserInput + GetSpecialCharacter$
     END IF
    END IF
   CASE ELSE
    IF KeyStroke >= " " THEN
     IF INSTR(Filter, KeyStroke) = 0 THEN
      IF Length < MaximumLength THEN UserInput = UserInput + KeyStroke
     END IF
    END IF
  END SELECT

  Length = LEN(UserInput)
 LOOP

 LOCATE x, y
 PRINT SPACE$(LEN(Prompt) + Length + 2);

 GetInput$ = UserInput
END FUNCTION

FUNCTION GetSpecialCharacter$
DIM Character AS STRING
DIM Column AS INTEGER
DIM KeyStroke AS STRING
DIM Row AS INTEGER

 Row = 0
 Column = 0

 PCOPY 0, 2
 COLOR 0, 7
 DrawBox 5, 21, 34, 8, -1, "Special character"
 DisplaySpecialCharacters

 DO
  Character = CHR$(127 + ((Row * 16) + Column))
  COLOR 23, 0
  LOCATE Row + 6, (Column * 2) + 22
  PRINT USING " ! "; Character;
 
  DO
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
 
  COLOR 0, 7
  LOCATE Row + 6, (Column * 2) + 22
  PRINT USING " ! "; Character;

  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF Row = 0 THEN Row = 7 ELSE Row = Row - 1
   CASE CHR$(0) + "P"
    IF Row = 7 THEN Row = 0 ELSE Row = Row + 1
   CASE CHR$(0) + "K"
    IF Column = 0 THEN Column = 15 ELSE Column = Column - 1
   CASE CHR$(0) + "M"
    IF Column = 15 THEN Column = 0 ELSE Column = Column + 1
   CASE CHR$(13)
    EXIT DO
   CASE CHR$(27)
    Character = ""
    EXIT DO
  END SELECT
 LOOP

 PCOPY 2, 0

 GetSpecialCharacter$ = Character
END FUNCTION

SUB Initialize (Selection AS SelectionStr, CurrentFileName AS STRING)
 CurrentFileName = ""
 Selection.Column = 0
 Selection.Page = 0
 Selection.Row = 0

 InitializeFileList

 SCREEN 0
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS
END SUB

SUB InitializeFileList
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN "Names.lst" FOR BINARY LOCK READ WRITE AS FileH
 CLOSE FileH
END SUB

FUNCTION LoadFile$ (AddressData() AS STRING)
DIM Column AS INTEGER
DIM FileH AS INTEGER
DIM FileName AS STRING
DIM Page AS INTEGER
DIM Row AS INTEGER

 PCOPY 0, 1
 COLOR 7, 0
 CLS
 COLOR 0, 7
 Center 1, " Load File "
 COLOR 7, 0
 PRINT STRING$(80, "Ä");

 FileName = GetInput$(4, 4, "Load file: ", 8, -1, 0)
 IF NOT FileName = "" THEN
  FileH = FREEFILE
  OPEN FileName + ".adr" FOR INPUT LOCK READ WRITE AS FileH
   FOR Page = LBOUND(AddressData, 3) TO UBOUND(AddressData, 3)
    FOR Row = LBOUND(AddressData, 1) TO UBOUND(AddressData, 1)
     FOR Column = LBOUND(AddressData, 2) TO UBOUND(AddressData, 2)
      IF FREEFILE <= FileH THEN EXIT FOR
      LINE INPUT #FileH, AddressData(Row, Column, Page)
      IF LEN(AddressData(Row, Column, Page)) > 14 THEN AddressData(Row, Column, Page) = LEFT$(AddressData(Row, Column, Page), 14)
     NEXT Column
     IF FREEFILE <= FileH THEN EXIT FOR
    NEXT Row
    IF FREEFILE <= FileH THEN EXIT FOR
   NEXT Page
  CLOSE FileH
 END IF

 PCOPY 1, 0

 LoadFile$ = FileName
END FUNCTION

SUB Main
DIM AddressData(0 TO 19, 0 TO 4, 0 TO 4) AS STRING
DIM ClipboardData AS STRING
DIM CurrentFileName AS STRING
DIM KeyStroke AS STRING
DIM Length AS INTEGER
DIM NewFileName AS STRING
DIM Selection AS SelectionStr

 Initialize Selection, CurrentFileName

 ClipboardData = ""

 COLOR 7, 0
 CLS
 DisplayTable AddressData(), Selection.Page
 DisplayInterface
 DO
  DisplayStatusBar Selection, CurrentFileName
  DisplayTableItem AddressData(), Selection, -1
  DO
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
  DisplayTableItem AddressData(), Selection, 0
  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF Selection.Row = LBOUND(AddressData, 1) THEN
     IF Selection.Page > LBOUND(AddressData, 3) THEN
      Selection.Page = Selection.Page - 1
      Selection.Row = UBOUND(AddressData, 1)
      DisplayTable AddressData(), Selection.Page
     END IF
    ELSE
     Selection.Row = Selection.Row - 1
    END IF
   CASE CHR$(0) + "P"
    IF Selection.Row = UBOUND(AddressData, 1) THEN
     IF Selection.Page < UBOUND(AddressData, 3) THEN
      Selection.Page = Selection.Page + 1
      Selection.Row = LBOUND(AddressData, 1)
      DisplayTable AddressData(), Selection.Page
     END IF
    ELSE
     Selection.Row = Selection.Row + 1
    END IF
   CASE CHR$(0) + "K"
    IF Selection.Column = LBOUND(AddressData, 2) THEN
     Selection.Column = UBOUND(AddressData, 2)
    ELSE
     Selection.Column = Selection.Column - 1
    END IF
   CASE CHR$(0) + "M"
    IF Selection.Column = UBOUND(AddressData, 2) THEN
     Selection.Column = LBOUND(AddressData, 2)
    ELSE
     Selection.Column = Selection.Column + 1
    END IF
   CASE CHR$(0) + "I"
    IF Selection.Page > LBOUND(AddressData, 3) THEN
     Selection.Page = Selection.Page - 1
     Selection.Row = UBOUND(AddressData, 1)
     DisplayTable AddressData(), Selection.Page
    END IF
   CASE CHR$(0) + "Q"
    IF Selection.Page < UBOUND(AddressData, 2) THEN
     Selection.Page = Selection.Page + 1
     Selection.Row = LBOUND(AddressData, 1)
     DisplayTable AddressData(), Selection.Page
    END IF
   CASE CHR$(0) + "S"
    IF NOT AddressData(Selection.Row, Selection.Column, Selection.Page) = "" THEN
     ClipboardData = AddressData(Selection.Row, Selection.Column, Selection.Page)
     AddressData(Selection.Row, Selection.Column, Selection.Page) = ""
    END IF
   CASE CHR$(0) + "R"
    AddressData(Selection.Row, Selection.Column, Selection.Page) = ClipboardData
   CASE CHR$(3)
    ClipboardData = AddressData(Selection.Row, Selection.Column, Selection.Page)
   CASE CHR$(6)
    Search AddressData(), CurrentFileName
   CASE CHR$(0) + ";"
    DisplayHelp
   CASE CHR$(0) + "<"
    NewFileName = LoadFile$(AddressData())
    IF NOT NewFileName = "" THEN CurrentFileName = NewFileName
    DisplayTable AddressData(), Selection.Page
   CASE CHR$(0) + "="
    NewFileName = SaveFileAs$(AddressData())
    IF NOT NewFileName = "" THEN CurrentFileName = NewFileName
   CASE CHR$(0) + ">"
    RemoveFile
   CASE CHR$(0) + "?"
    LOCATE 23, 1
    PRINT SPACE$(80);
    IF GetChoice$(23, 4, "Remove all files Y/N?", "YN") = "Y" THEN
     DeleteFiles
    END IF
   CASE CHR$(0) + "@"
    DisplayFileList
   CASE CHR$(0) + "A"
    PrintTable AddressData(), Selection
   CASE CHR$(0) + "B"
    IF Length < 14 THEN
     AddressData(Selection.Row, Selection.Column, Selection.Page) = AddressData(Selection.Row, Selection.Column, Selection.Page) + GetSpecialCharacter$
    END IF
   CASE CHR$(0) + "C"
    LOCATE 23, 1
    PRINT SPACE$(80);
    IF GetChoice$(23, 4, "Quit Y/N?", "YN") = "Y" THEN Quit
   CASE CHR$(8)
    IF Length > 0 THEN
     AddressData(Selection.Row, Selection.Column, Selection.Page) = LEFT$(AddressData(Selection.Row, Selection.Column, Selection.Page), Length - 1)
    END IF
   CASE CHR$(14)
    LOCATE 23, 1
    PRINT SPACE$(80);
    IF GetChoice$(23, 4, "Begin with new file Y/N?", "YN") = "Y" THEN
     Initialize Selection, CurrentFileName
     REDIM AddressData(0 TO 19, 0 TO 4, 0 TO 4) AS STRING
     DisplayInterface
    END IF
    DisplayTable AddressData(), Selection.Page
   CASE CHR$(19)
    IF CurrentFileName = "" THEN
     NewFileName = SaveFileAs$(AddressData())
     IF NOT NewFileName = "" THEN CurrentFileName = NewFileName
    ELSE
     SaveFile AddressData(), CurrentFileName
    END IF
   CASE ELSE
    IF KeyStroke >= " " THEN
     IF Length < 14 THEN
      AddressData(Selection.Row, Selection.Column, Selection.Page) = AddressData(Selection.Row, Selection.Column, Selection.Page) + KeyStroke
     END IF
    END IF
  END SELECT

  Length = LEN(AddressData(Selection.Row, Selection.Column, Selection.Page))
 LOOP
END SUB

FUNCTION PrintDialog (Selection AS SelectionStr, FirstPage AS INTEGER, LastPage AS INTEGER)
DIM Continue AS INTEGER

 Continue = -1

 PCOPY 0, 1
 COLOR 7, 0
 CLS
 COLOR 0, 7
 Center 1, " Print "
 COLOR 7, 0
 PRINT STRING$(80, "Ä");

 SELECT CASE GetChoice$(4, 4, "1 = Print all.  2 = Print one page.  3 = Cancel.", "123")
  CASE "1"
   FirstPage = 0
   LastPage = 4
  CASE "2"
   FirstPage = Selection.Page
   LastPage = Selection.Page
  CASE "3"
   Continue = 0
 END SELECT

 PCOPY 1, 0

 PrintDialog = Continue
END FUNCTION

SUB PrintTable (AddressData() AS STRING, Selection AS SelectionStr)
DIM Column AS INTEGER
DIM FirstPage AS INTEGER
DIM LastPage AS INTEGER
DIM Page AS INTEGER
DIM Row AS INTEGER
DIM RowData AS STRING
DIM RowsPrinted AS INTEGER

 IF PrintDialog(Selection, FirstPage, LastPage) THEN
  LPRINT " Name:           Address:        City:           Postal Code:    Phone Number:  ";

  FOR Page = FirstPage TO LastPage
   IF RowsPrinted > 0 OR Page = 1 THEN
    LPRINT STRING$(80, "=");
   END IF
   RowsPrinted = 0
   FOR Row = LBOUND(AddressData, 1) TO UBOUND(AddressData, 1)
    RowData = ""
    FOR Column = LBOUND(AddressData, 2) TO UBOUND(AddressData, 2)
     RowData = RowData + " " + AddressData(Row, Column, Page) + SPACE$(15 - LEN(AddressData(Row, Column, Page)))
    NEXT Column
    IF NOT LTRIM$(RTRIM$(RowData)) = "" THEN
     LPRINT RowData;
     RowsPrinted = RowsPrinted + 1
    END IF
   NEXT Row
  NEXT Page
 END IF

 PCOPY 1, 0
END SUB

SUB Quit
 SCREEN 0
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS
 SYSTEM
END SUB

SUB RemoveFile
DIM FileName AS STRING
DIM RemovedFile AS STRING
DIM SourceFileH AS INTEGER
DIM TargetFileH AS INTEGER

 PCOPY 0, 1
 COLOR 7, 0
 CLS
 COLOR 0, 7
 Center 1, " Remove File "
 COLOR 7, 0
 PRINT STRING$(80, "Ä");

 RemovedFile = UCASE$(GetInput$(4, 4, "Remove file: ", 8, -1, 0))

 IF NOT RemovedFile = "" THEN
  IF GetChoice$(4, 4, "Remove " + RemovedFile + " Y/N?", "YN") = "Y" THEN
   SourceFileH = FREEFILE
   OPEN "Names.lst" FOR INPUT LOCK READ WRITE AS SourceFileH
    TargetFileH = FREEFILE
    OPEN "Names.tmp" FOR OUTPUT LOCK READ WRITE AS TargetFileH
     DO UNTIL EOF(SourceFileH)
      LINE INPUT #SourceFileH, FileName
      IF NOT FileName = RemovedFile THEN PRINT #2, FileName
     LOOP
    CLOSE TargetFileH
   CLOSE SourceFileH

   KILL "Names.lst"
   NAME "Names.tmp" AS "Names.lst"
   KILL RemovedFile + ".adr"
  END IF
 END IF

 PCOPY 1, 0
END SUB

SUB SaveFile (AddressData() AS STRING, FileName AS STRING)
DIM Column AS INTEGER
DIM FileH AS INTEGER
DIM Page AS INTEGER
DIM Row AS INTEGER

 FileH = FREEFILE
 OPEN FileName + ".adr" FOR OUTPUT LOCK READ WRITE AS FileH
  FOR Page = LBOUND(AddressData, 3) TO UBOUND(AddressData, 3)
   FOR Row = LBOUND(AddressData, 1) TO UBOUND(AddressData, 1)
    FOR Column = LBOUND(AddressData, 2) TO UBOUND(AddressData, 2)
     PRINT #FileH, AddressData(Row, Column, Page)
    NEXT Column
   NEXT Row
  NEXT Page
 CLOSE FileH

END SUB

FUNCTION SaveFileAs$ (AddressData() AS STRING)
DIM FileName AS STRING

 PCOPY 0, 1
 COLOR 7, 0
 CLS
 COLOR 0, 7
 Center 1, " Save File "
 COLOR 7, 0
 PRINT STRING$(80, "Ä");

 FileName = GetInput$(4, 4, "Save file: ", 8, -1, 0)
 IF NOT FileName = "" THEN
  IF FileExists(FileName) THEN
   IF GetChoice$(4, 4, "File already exists, overwrite Y/N?", "YN") = "Y" THEN
    SaveFile AddressData(), FileName
   END IF
  ELSE
   AddFileNameToList FileName
   SaveFile AddressData(), FileName
  END IF
 END IF

 PCOPY 1, 0

 SaveFileAs$ = FileName
END FUNCTION

SUB Search (AddressData() AS STRING, FileName AS STRING)
DIM Cancel AS INTEGER
DIM KeyStroke AS STRING
DIM NamesFileH AS INTEGER
DIM SearchFile AS STRING
DIM SearchFileH AS INTEGER
DIM SearchText AS STRING
DIM TextFound AS INTEGER

 PCOPY 0, 1
 COLOR 7, 0
 CLS
 COLOR 0, 7
 Center 1, " Search "
 COLOR 7, 0
 PRINT STRING$(80, "Ä");

 SearchText = GetInput$(4, 4, "Search for: ", 14, 0, -1)
 IF NOT SearchText = "" THEN
  Cancel = 0
  TextFound = 0
  SELECT CASE GetChoice$(4, 4, "1 = Search this file.  2 = Search all files.", "12")
   CASE "1"
    SearchFile = FileName
    Cancel = SearchData(AddressData(), SearchText, "", 0, TextFound)
   CASE "2"
    NamesFileH = FREEFILE
    OPEN "Names.lst" FOR INPUT LOCK READ WRITE AS NamesFileH
     DO UNTIL EOF(NamesFileH) OR Cancel
      LINE INPUT #NamesFileH, SearchFile
      IF NOT LTRIM$(RTRIM$(SearchFile)) = "" THEN
       SearchFileH = FREEFILE
       OPEN SearchFile + ".adr" FOR INPUT LOCK READ WRITE AS SearchFileH
        Cancel = SearchData(AddressData(), SearchText, SearchFile, SearchFileH, TextFound)
       CLOSE SearchFileH
      END IF
     LOOP
    CLOSE NamesFileH
  END SELECT

  IF NOT TextFound THEN
   COLOR 7, 0
   LOCATE 4, 4
   PRINT "Text not found."
   KeyStroke = INPUT$(1)
  END IF
 END IF

 PCOPY 1, 0
END SUB

FUNCTION SearchData (AddressData() AS STRING, SearchText AS STRING, SearchFile AS STRING, SearchFileH AS INTEGER, TextFound AS INTEGER)
DIM Cancel AS INTEGER
DIM Column AS INTEGER
DIM Page AS INTEGER
DIM Row AS INTEGER
DIM SearchedData AS STRING
DIM SearchResult AS SelectionStr

 Cancel = 0
 FOR Page = LBOUND(AddressData, 3) TO UBOUND(AddressData, 3)
  FOR Row = LBOUND(AddressData, 1) TO UBOUND(AddressData, 1)
   FOR Column = LBOUND(AddressData, 2) TO UBOUND(AddressData, 2)
    IF NOT SearchFileH = 0 THEN
     IF EOF(SearchFileH) THEN
      CLOSE SearchFileH
      Cancel = -1
      EXIT FOR
     END IF
     LINE INPUT #SearchFileH, SearchedData
    ELSE
     SearchedData = AddressData(Row, Column, Page)
    END IF

    IF INSTR(UCASE$(LTRIM$(RTRIM$(SearchedData))), UCASE$(LTRIM$(RTRIM$(SearchText)))) > 0 THEN
     TextFound = -1
     SearchResult.Column = Column
     SearchResult.Page = Page
     SearchResult.Row = Row
     Cancel = SearchDialog(SearchText, SearchFile, SearchResult)
     IF Cancel THEN EXIT FOR
    END IF
   NEXT Column
   IF Cancel THEN EXIT FOR
  NEXT Row
  IF Cancel THEN EXIT FOR
 NEXT Page

 SearchData = Cancel
END FUNCTION

FUNCTION SearchDialog (SearchText AS STRING, SearchFile AS STRING, SearchResult AS SelectionStr)
DIM Cancel AS INTEGER
DIM KeyStroke AS STRING
 
 Cancel = 0
 COLOR 0, 7
 DrawBox 5, 25, 28, 5, -1, "Search"
 LOCATE 6, 27: PRINT USING "Search text: \            \"; SearchText
 LOCATE , 27: PRINT USING "       File: \      \"; SearchFile
 LOCATE , 27: PRINT USING "       Page: ##"; SearchResult.Page + 1
 LOCATE , 27: PRINT USING "        Row: ##"; SearchResult.Row + 1
 LOCATE , 27: PRINT USING "     Column: ##"; SearchResult.Column + 1

 COLOR 7, 0
 LOCATE 23, 3
 PRINT "Escape = Cancel   Any other key = Continue"

 KeyStroke = INPUT$(1)
 Cancel = (KeyStroke = CHR$(27))

 SearchDialog = Cancel
END FUNCTION

