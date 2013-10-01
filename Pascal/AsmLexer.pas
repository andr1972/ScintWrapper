unit AsmLexer;

interface

uses
  Graphics, Scintilla;

type
  TAsmLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation
const
  sampleLines: AnsiString = //edited from http://assembly.happycodings.com/code1.html
 'main:'#10+
'; initializes the two numbers and the counter.  Note that this assumes'#10+
'; that the counter and num1 and num2 areas are contiguous!'#10+
';'#10+
'  mov  ax,''00''    ; initialize to all ASCII zeroes'#10+
'  mov  di,counter    ; including the counter'#10+
'  mov  cx,digits+cntDigits/2  ; two bytes at a time'#10+
'  cld      ; initialize from low to high memory'#10+
'  rep  stosw    ; write the data'#10+
'  inc  ax    ; make sure ASCII zero is in al'#10+
'  mov  [num1 + digits - 1],al ; last digit is one'#10+
'  mov  [num2 + digits - 1],al ;'#10+
'  mov  [counter + cntDigits - 1],al'#10+
#10+
'  jmp  .bottom    ; done with initialization, so begin'#10+
#10+
'.top'; 

function TAsmLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TAsmLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_ASM);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  SetAStyle(SCE_ASM_COMMENT, TColor($808000));
  SetAStyle(SCE_ASM_COMMENTBLOCK, TColor($808000));
  SetAStyle(SCE_ASM_NUMBER, clBlue);
  SetAStyle(SCE_ASM_STRING, clFuchsia);
  SetAStyle(SCE_ASM_OPERATOR, clMaroon);
  SetAStyle(SCE_ASM_IDENTIFIER, TColor($800000));
  SetAStyle(SCE_ASM_CPUINSTRUCTION, TColor($005020));
  SetAStyle(SCE_ASM_MATHINSTRUCTION, TColor($005070));
  SetAStyle(SCE_ASM_REGISTER, TColor($0040E0));
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_ASM_REGISTER, 1);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.asm')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
