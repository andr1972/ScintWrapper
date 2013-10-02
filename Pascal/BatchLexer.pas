unit BatchLexer;

interface

uses
  Graphics, Scintilla;

type
  TBatchLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;

const
  sampleLines: AnsiString = //https://en.wikipedia.org/wiki/Batch_file
'@echo off'#10+
'color 0E'#10+
'title Conditional Shutdown.'#10+
#10+
'rem comment'#10+
':start'#10+
'echo Welcome, %USERNAME%'#10+
'echo What would you like to do?'#10+
'echo.'#10+
'echo 1. Shut down within specified number of minutes'#10+
'echo 2. Shut down at a specified time'#10+
'echo 3. Shut down now'#10+
'echo 4. Restart now'#10+
'echo 5. Log off now'#10+
'echo 6. Hibernate now'#10+
'echo 7. Cancel shutdown'#10+
'echo.'#10+
'echo 0. Quit'#10+
'echo.'#10+
#10+
'set /p choice="Enter your choice: "'#10+
'if "%choice%"=="1" goto shutdown'#10+
'if "%choice%"=="2" goto shutdown-clock'#10+
'if "%choice%"=="3" shutdown.exe -s -f'#10+
'if "%choice%"=="4" shutdown.exe -r -f'#10+
'if "%choice%"=="5" shutdown.exe -l -f'#10+
'if "%choice%"=="6" shutdown.exe -h -f'#10+
'if "%choice%"=="7" goto cancel_now'#10+
'if "%choice%"=="0" exit'#10+
'echo Invalid choice: %choice%'#10+
'echo.'#10+
'pause'#10+
'cls'#10+
'goto start'#10+
#10+
':shutdown'#10+
'cls'#10+
'set /p min="Minutes until shutdown: "'#10+
'set /a sec=60*%min%'#10+
'shutdown.exe -s -f -t %sec%'#10+
'echo Shutdown initiated at %time%'#10+
'echo.'#10+
'goto cancel'#10+
#10+
':shutdown-clock'#10+
'echo.'#10+
'echo The time format is HH:MM:SS (24 hour time)'#10+
'echo Example: 14:30:00 for 2:30 PM'#10+
'echo.'#10+
'set /p tmg=Enter the time at which you wish the computer to shut down:'#10+
'schtasks.exe /create /sc ONCE /tn shutdown /st %tmg% /tr "shutdown.exe -s -t 00"'#10+
'echo Shutdown initiated at %tmg%'#10+
'echo.'#10+
#10+
':cancel'#10+
'set /p cancel="Type cancel to stop shutdown: "'#10+
'if not "%cancel%"=="cancel" exit'#10+
':cancel_now'#10+
'shutdown.exe -a'#10+
'cls'#10+
'schtasks.exe /end /tn shutdown'#10+
'cls'#10+
'schtasks.exe /delete /tn shutdown'#10+
'cls'#10+
'echo Any impending shutdown has been cancelled.'#10+
'echo.'#10+
'pause'#10+
'exit';

const batchKeyWords: PAnsiChar =
    'call cd cls copy del do echo errorlevel exist'+
    'for goto if in not off on pause set shift'+
    'start title';

function TBatchLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TBatchLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_BATCH);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(batchKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  SetAStyle(SCE_BAT_COMMENT, TColor($808000));
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_BAT_WORD, 1);
  SetAStyle(SCE_BAT_WORD, TColor($005050));
  SetAStyle(SCE_BAT_LABEL, clRed);
  SetAStyle(SCE_BAT_HIDE, clGreen);
  SetAStyle(SCE_BAT_COMMAND, darkBlue);
  SetAStyle(SCE_BAT_IDENTIFIER, clBlue);
  SetAStyle(SCE_BAT_OPERATOR, clNavy);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.batch')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
