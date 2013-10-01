unit LatexLexer;

interface

uses
  Graphics, Scintilla;

type
  TLatexLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

const
  sampleLines: AnsiString =
'\documentclass[final,letterpaper,twoside,12pt]{article}'#10+
'% if you use "report", you get a seperate title page'#10+
'%\documentclass[final,letterpaper,twoside,12pt]{report}'#10+
'%'#10+
#10+
#10+
'\author{Terry~Sturtevant \thanks{Physics Lab Supervisor}}'#10+
'\date{\today}'#10+
'\title{PC132 Lab Project}'#10+
#10+
'\begin{document}'#10+
'\maketitle'#10+
#10+
'\section{Very Important stuff}'#10+
'Here is my text, including the \LaTeX\ symbol.'#10+
'\subsection{Important stuff}'#10+
'More text here.'#10+
'\subsubsection{Less Important stuff}'#10+
'And even more text.'#10+
'\paragraph{Trivial stuff}'#10+
'Note that the text starts on the same line as the paragraph heading.'#10+
'\subparagraph{Mind-numbingly inconsequential stuff}'#10+
#10+
'Even if you skip lines.'#10+
#10+
'Just skipping a line starts a new paragraph.'#10+
#10+
'\noindent You can avoid an indent by using the \textbf{{$\mathbf \backslash$}noindent} command.'#10+
#10+
'\section*{Very Important stuff with no number}'#10+
'Here is my text, including the \LaTeX\ symbol.'#10+
#10+
'  \subsection*{Important stuff with no number}'#10+
'  Note neither the indent nor the blank line affects the output.'#10+
#10+
'    \subsubsection*{Less Important stuff with no number} The text'#10+
'    can even begin on the same line.'#10+
#10+
'      \paragraph*{Why bother?}'#10+
'      Note the indenting in the source file doesn''t'#10+
'        show up in the output, so it''s for your'#10+
'      benefit only.'#10+
'        \subparagraph*{Mind-numbingly inconsequential stuff}'#10+
'        Note which types of headings get numbered and/or highlighted.'#10+
#10+
'        Just skipping a line starts a new paragraph.'#10+
#10+
'\end{document}';


function TLatexLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TLatexLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_LATEX);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_L_COMMAND, 1);
  SetAStyle(SCE_L_COMMAND, darkBlue);
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_L_TAG, 1);
  SetAStyle(SCE_L_TAG, darkGreen);
  SetAStyle(SCE_L_MATH, offWhite);
  SetAStyle(SCE_L_COMMENT, TColor($808000));
  SetAStyle(SCE_L_COMMENT2, TColor($808000));
  SetAStyle(SCE_L_TAG2, clGreen);
  SetAStyle(SCE_L_MATH2, offWhite);
  SetAStyle(SCE_L_VERBATIM, clLime);
  SetAStyle(SCE_L_SHORTCMD, clLtGray);
  SetAStyle(SCE_L_SPECIAL, clBlue);
  SetAStyle(SCE_L_CMDOPT, clNavy);
  SetAStyle(SCE_L_ERROR, clRed);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.latex')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
