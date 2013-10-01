unit DefaultLexer;

interface

uses
  Graphics, Scintilla;

type
  TDefaultLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

const
  sampleLines: AnsiString =
 'Lorem ipsum dolor sit amet, consectetur adipisicing elit.'#10+
 'Proin nibh augue, suscipit a, scelerisque sed, lacinia in, mi.'#10+
 'Cras vel lorem. Etiam pellentesque aliquet tellus. Phasellus pharetra nulla ac diam.'#10+
 'Quisque semper justo at risus.'#10+
 'Donec venenatis, turpis vel hendrerit interdum, dui ligula ultricies purus, sed posuere libero dui id orci.'#10+
 'Nam congue, pede vitae dapibus aliquet, elit magna vulputate arcu, vel tempus metus leo non est.'#10+
 'Etiam sit amet lectus quis est congue mollis. Phasellus congue lacus eget neque.'#10+
 'Phasellus ornare, ante vitae consectetuer consequat, purus sapien ultricies dolor, et mollis pede metus eget nisi.'#10+
 'Praesent sodales velit quis augue.'#10+
 'Cras suscipit, urna at aliquam rhoncus, urna quam viverra nisi, in interdum massa nibh nec erat.';

function TDefaultLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TDefaultLexer.InitDefaults;
begin
  FOwner.ClearDocumentStyle;
  FOwner.SendEditor(SCI_STYLESETFORE, STYLE_DEFAULT, clBlack);
  FOwner.SendEditor(SCI_STYLESETBACK, STYLE_DEFAULT, clWhite);
  FOwner.SendEditor(SCI_STYLESETSIZE, STYLE_DEFAULT, 10);
  FOwner.SendEditor(SCI_STYLESETFONT, STYLE_DEFAULT, integer(PAnsiChar('Courier New')));
end;

end.
