unit LispLexer;

interface

uses
  Graphics, Scintilla;

type
  TLispLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation
const
  sampleLines: AnsiString = //edits from http://forrest.psych.unc.edu/teaching/p285/p285pgmexmpchap3.html
';One way of doing comments (line comments)'#10+
';-=-=-=-=-=-=-=-=-=-=-='#10+
';Section 3.5 Iteration and Recursion'#10+
';-=-=-=-=-=-=-=-=-=-=-='#10+
';'#10+
'(defun sqrt-iter (guess x)'#10+
'  (if (good-enough-p guess x)'#10+
'      guess'#10+
'      (sqrt-iter (improve guess x) x)))'#10+
#10+
'(defun improve (guess x)'#10+
'  (mean (list guess (/ x guess))))'#10+
#10+
'#| Another way of doing comments (block comments)'#10+
'-=-=-=-=-=-=-=-=-=-=-='#10+
'Section 3.5 Scope of definition'#10+
'-=-=-=-=-=-=-=-=-=-=-='#10+
'|#';


lispKeyWords: PAnsiChar = 'defparameter defun if nil';

function TLispLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TLispLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_LISP);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(lispKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others
  SetAStyle(SCE_LISP_COMMENT, TColor($808000));
  SetAStyle(SCE_LISP_MULTI_COMMENT, TColor($808000));
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_LISP_KEYWORD, 1);
  SetAStyle(SCE_LISP_KEYWORD, clBlue);
  SetAStyle(SCE_LISP_NUMBER, TColor(005060));
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.lisp')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
