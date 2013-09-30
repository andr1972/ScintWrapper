unit HtmlLexer;

interface

uses
  Graphics, Scintilla;

type
  THtmlLexer = class(TLexer)
  protected
  public
    procedure InitDefaults; override;
  end;

implementation

{ THtmlLexer }

const htmlKeyWords: PAnsiChar =
	'a abbr acronym address applet area b base basefont '+
	'bdo big blockquote body br button caption center '+
	'cite code col colgroup dd del dfn dir div dl dt em '+
	'fieldset font form frame frameset h1 h2 h3 h4 h5 h6 '+
	'head hr html i iframe img input ins isindex kbd label '+
	'legend li link map menu meta noframes noscript '+
	'object ol optgroup option p param pre q s samp '+
	'script select small span strike strong style sub sup '+
	'table tbody td textarea tfoot th thead title tr tt u ul '+
	'var xmlns '+
	'abbr accept-charset accept accesskey action align alink '+
	'alt archive axis background bgcolor border '+
	'cellpadding cellspacing char charoff charset checked cite '+
	'class classid clear codebase codetype color cols colspan '+
	'compact content coords '+
	'data datafld dataformatas datapagesize datasrc datetime '+
	'declare defer dir disabled enctype '+
	'face for frame frameborder '+
	'headers height href hreflang hspace http-equiv '+
	'id ismap label lang language link longdesc '+
	'marginwidth marginheight maxlength media method multiple '+
	'name nohref noresize noshade nowrap '+
	'object onblur onchange onclick ondblclick onfocus '+
	'onkeydown onkeypress onkeyup onload onmousedown '+
	'onmousemove onmouseover onmouseout onmouseup '+
	'onreset onselect onsubmit onunload '+
	'profile prompt readonly rel rev rows rowspan rules '+
	'scheme scope shape size span src standby start style '+
	'summary tabindex target text title type usemap '+
	'valign value valuetype version vlink vspace width '+
	'text password checkbox radio submit reset '+
	'file hidden image '+
	'public !doctype xml';

 jsKeyWords: PAnsiChar =
	'break case catch continue default '+
	'do else for function if return throw try var while';

 vbsKeyWords: PAnsiChar =
	'and as byref byval case call const '+
	'continue dim do each else elseif end error exit false for function global '+
	'goto if in loop me new next not nothing on optional or private public '+
	'redim rem resume select set sub then to true type while with '+
	'boolean byte currency date double integer long object single string type '+
	'variant';

procedure THtmlLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_HTML);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(htmlKeyWords));
  FOwner.SendEditor(SCI_SETKEYWORDS, 1, integer(jsKeyWords));
  FOwner.SendEditor(SCI_SETKEYWORDS, 2, integer(vbsKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  // Hypertext default is used for all the document's text
  SetAStyle(SCE_H_DEFAULT, clBlack);

  // Unknown tags and attributes are highlighed in red.
  // If a tag is actually OK, it should be added in lower case to the htmlKeyWords string.
  SetAStyle(SCE_H_TAG, darkBlue);
  SetAStyle(SCE_H_TAGUNKNOWN, red);
  SetAStyle(SCE_H_ATTRIBUTE, darkBlue);
  SetAStyle(SCE_H_ATTRIBUTEUNKNOWN, red);
  SetAStyle(SCE_H_NUMBER, TColor($800080));
  SetAStyle(SCE_H_DOUBLESTRING, TColor($008000));
  SetAStyle(SCE_H_SINGLESTRING, TColor($008000));
  SetAStyle(SCE_H_OTHER, TColor($800080));
  SetAStyle(SCE_H_COMMENT, TColor($808000));
  SetAStyle(SCE_H_ENTITY, TColor($800080));

  SetAStyle(SCE_H_TAGEND, darkBlue);
  SetAStyle(SCE_H_XMLSTART, darkBlue);	// <?
  SetAStyle(SCE_H_XMLEND, darkBlue);		// ?>
  SetAStyle(SCE_H_SCRIPT, darkBlue);		// <script
  SetAStyle(SCE_H_ASP, TColor($4F4F00), TColor($FFFF00));	// <% ... %>
  SetAStyle(SCE_H_ASPAT, TColor($4F4F00), TColor($FFFF00));	// <%@ ... %>

  SetAStyle(SCE_HB_DEFAULT, clBlack);
  SetAStyle(SCE_HB_COMMENTLINE, darkGreen);
  SetAStyle(SCE_HB_NUMBER, TColor($008080));
  SetAStyle(SCE_HB_WORD, darkBlue);
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_HB_WORD, 1);
  SetAStyle(SCE_HB_STRING, TColor($800080));
  SetAStyle(SCE_HB_IDENTIFIER, clBlack);

  // This light blue is found in the windows system palette so is safe to use even in 256 colour modes.
  lightBlue := TColor($A6CAF0);
	// Show the whole section of VBScript with light blue background
	for bstyle:=SCE_HB_DEFAULT to SCE_HB_STRINGEOL do
        begin
		FOwner.SendEditor(SCI_STYLESETFONT, bstyle,
			integer(PAnsiChar('Georgia')));
		FOwner.SendEditor(SCI_STYLESETBACK, bstyle, lightBlue);
		// This call extends the backround colour of the last style on the line to the edge of the window
		FOwner.SendEditor(SCI_STYLESETEOLFILLED, bstyle, 1);
        end;
	FOwner.SendEditor(SCI_STYLESETBACK, SCE_HB_STRINGEOL, TColor($7F7FFF));
	FOwner.SendEditor(SCI_STYLESETFONT, SCE_HB_COMMENTLINE,
		integer(PAnsiChar('Comic Sans MS')));

	SetAStyle(SCE_HBA_DEFAULT, clBlack);
	SetAStyle(SCE_HBA_COMMENTLINE, darkGreen);
	SetAStyle(SCE_HBA_NUMBER, TColor($008080));
	SetAStyle(SCE_HBA_WORD, darkBlue);
	FOwner.SendEditor(SCI_STYLESETBOLD, SCE_HBA_WORD, 1);
	SetAStyle(SCE_HBA_STRING, TColor($800080));
	SetAStyle(SCE_HBA_IDENTIFIER, clBlack);

	// Show the whole section of ASP VBScript with bright yellow background
	for bastyle:=SCE_HBA_DEFAULT to SCE_HBA_STRINGEOL do
        begin
		FOwner.SendEditor(SCI_STYLESETFONT, bastyle,
			integer(PAnsiChar('Georgia')));
		FOwner.SendEditor(SCI_STYLESETBACK, bastyle, TColor($FFFF00));
		// This call extends the backround colour of the last style on the line to the edge of the window
		FOwner.SendEditor(SCI_STYLESETEOLFILLED, bastyle, 1);
        end;
	FOwner.SendEditor(SCI_STYLESETBACK, SCE_HBA_STRINGEOL, TColor($CFCF7F));
	FOwner.SendEditor(SCI_STYLESETFONT, SCE_HBA_COMMENTLINE,
		integer(PAnsiChar('Comic Sans MS')));

	// If there is no need to support embedded Javascript, the following code can be dropped.
	// Javascript will still be correctly processed but will be displayed in just the default style.

	SetAStyle(SCE_HJ_START, TColor($808000));
	SetAStyle(SCE_HJ_DEFAULT, clBlack);
	SetAStyle(SCE_HJ_COMMENT, darkGreen);
	SetAStyle(SCE_HJ_COMMENTLINE, darkGreen);
	SetAStyle(SCE_HJ_COMMENTDOC, darkGreen);
	SetAStyle(SCE_HJ_NUMBER, TColor($008080));
	SetAStyle(SCE_HJ_WORD, clBlack);
	SetAStyle(SCE_HJ_KEYWORD, darkBlue);
	SetAStyle(SCE_HJ_DOUBLESTRING, TColor($800080));
	SetAStyle(SCE_HJ_SINGLESTRING, TColor($800080));
	SetAStyle(SCE_HJ_SYMBOLS, clBlack);

	SetAStyle(SCE_HJA_START, TColor($808000));
	SetAStyle(SCE_HJA_DEFAULT, clBlack);
	SetAStyle(SCE_HJA_COMMENT, darkGreen);
	SetAStyle(SCE_HJA_COMMENTLINE, darkGreen);
	SetAStyle(SCE_HJA_COMMENTDOC, darkGreen);
	SetAStyle(SCE_HJA_NUMBER, TColor($008080));
	SetAStyle(SCE_HJA_WORD, clBlack);
	SetAStyle(SCE_HJA_KEYWORD, darkBlue);
	SetAStyle(SCE_HJA_DOUBLESTRING, TColor($800080));
	SetAStyle(SCE_HJA_SINGLESTRING, TColor($800080));
	SetAStyle(SCE_HJA_SYMBOLS, clBlack);

	// Show the whole section of Javascript with off white background
	for jstyle:=SCE_HJ_DEFAULT to SCE_HJ_SYMBOLS do
        begin
		FOwner.SendEditor(SCI_STYLESETFONT, jstyle,
			integer(PAnsiChar('Lucida Sans Unicode')));
		FOwner.SendEditor(SCI_STYLESETBACK, jstyle, offWhite);
		FOwner.SendEditor(SCI_STYLESETEOLFILLED, jstyle, 1);
        end;
	FOwner.SendEditor(SCI_STYLESETBACK, SCE_HJ_STRINGEOL, TColor($DFDF7F));
	FOwner.SendEditor(SCI_STYLESETEOLFILLED, SCE_HJ_STRINGEOL, 1);

	// Show the whole section of Javascript with brown background
	for jastyle:=SCE_HJA_DEFAULT to SCE_HJA_SYMBOLS do
        begin
		FOwner.SendEditor(SCI_STYLESETFONT, jastyle,
			integer(PAnsiChar('Lucida Sans Unicode')));
		FOwner.SendEditor(SCI_STYLESETBACK, jastyle, TColor($DFDF7F));
		FOwner.SendEditor(SCI_STYLESETEOLFILLED, jastyle, 1);
        end;
	FOwner.SendEditor(SCI_STYLESETBACK, SCE_HJA_STRINGEOL, TColor($00AF5F));
	FOwner.SendEditor(SCI_STYLESETEOLFILLED, SCE_HJA_STRINGEOL, 1);

        FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.html')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
