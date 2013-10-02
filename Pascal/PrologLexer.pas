unit PrologLexer;

interface

uses
  Graphics, Scintilla;

type
  TPrologLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;
const
  sampleLines: AnsiString = //http://www.cs.toronto.edu/~hojjat/384w09/simple-prolog-examples.html
'% move(N,X,Y,Z) - move N disks from peg X to peg Y, with peg Z being the'#10+
'%                 auxilliary peg'#10+
'%'#10+
'% Strategy:'#10+
'% Base Case: One disc - To transfer a stack consisting of 1 disc from'#10+
'%    peg X to peg Y, simply move that disc from X to Y'#10+
'% Recursive Case: To transfer n discs from X to Y, do the following:'#10+
'         Transfer the first n-1 discs to some other peg X'#10+
'         Move the last disc on X to Y'#10+
'         Transfer the n-1 discs from X to peg Y'#10+
#10+
'     move(1,X,Y,_) :-'#10+
'         write(''Move top disk from ''),'#10+
'         write(X),'#10+
'         write('' to ''),'#10+
'         write(Y),'#10+
'         nl.'#10+
'     move(N,X,Y,Z) :-'#10+
'         N>1,'#10+
'         M is N-1,'#10+
'         move(M,X,Z,Y),'#10+
'         move(1,X,Y,_),'#10+
'         move(M,Z,Y,X).';
function TPrologLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TPrologLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_VISUALPROLOG);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others
  SetAStyle(SCE_VISUALPROLOG_COMMENT_BLOCK, TColor($808000));
  SetAStyle(SCE_VISUALPROLOG_COMMENT_LINE, TColor($808000));
  SetAStyle(SCE_VISUALPROLOG_COMMENT_KEY, TColor($808000));
  SetAStyle(SCE_VISUALPROLOG_COMMENT_KEY_ERROR, TColor($808000));
  SetAStyle(SCE_VISUALPROLOG_IDENTIFIER, TColor($800000));
  SetAStyle(SCE_VISUALPROLOG_VARIABLE, TColor($007040));
  SetAStyle(SCE_VISUALPROLOG_CHARACTER, clMaroon);
  SetAStyle(SCE_VISUALPROLOG_CHARACTER_TOO_MANY, clMaroon);
  SetAStyle(SCE_VISUALPROLOG_CHARACTER_ESCAPE_ERROR, clMaroon);
  SetAStyle(SCE_VISUALPROLOG_STRING, clFuchsia);
  SetAStyle(SCE_VISUALPROLOG_STRING_ESCAPE, clFuchsia);
  SetAStyle(SCE_VISUALPROLOG_STRING_ESCAPE_ERROR, clFuchsia);
  SetAStyle(SCE_VISUALPROLOG_STRING_EOL_OPEN, clFuchsia);
  SetAStyle(SCE_VISUALPROLOG_STRING_VERBATIM, clFuchsia);
  SetAStyle(SCE_VISUALPROLOG_STRING_VERBATIM_SPECIAL, clFuchsia);
  SetAStyle(SCE_VISUALPROLOG_STRING_VERBATIM_EOL, clFuchsia);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.visualprolog')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
